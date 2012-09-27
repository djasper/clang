//=- IvarInvalidationChecker.cpp - -*- C++ ----*-==//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This checker implements annotation driven invalidation checking. If a class
//  contains a method annotated with 'objc_instance_variable_invalidator',
//  - (void) foo
//           __attribute__((annotate("objc_instance_variable_invalidator")));
//  all the "ivalidatable" instance variables of this class should be
//  invalidated. We call an instance variable ivalidatable if it is an object of
//  a class which contains an invalidation method.
//
//  Note, this checker currently only checks if an ivar was accessed by the
//  method, we do not currently support any deeper invalidation checking.
//
//===----------------------------------------------------------------------===//

#include "ClangSACheckers.h"
#include "clang/StaticAnalyzer/Core/Checker.h"
#include "clang/StaticAnalyzer/Core/BugReporter/BugReporter.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/AnalysisManager.h"
#include "clang/AST/DeclObjC.h"
#include "clang/AST/StmtVisitor.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallString.h"

using namespace clang;
using namespace ento;

namespace {
class IvarInvalidationChecker :
  public Checker<check::ASTDecl<ObjCMethodDecl> > {

  typedef llvm::DenseMap<const ObjCIvarDecl*, bool> IvarSet;
  typedef llvm::DenseMap<const ObjCMethodDecl*,
                         const ObjCIvarDecl*> MethToIvarMapTy;
  typedef llvm::DenseMap<const ObjCPropertyDecl*,
                         const ObjCIvarDecl*> PropToIvarMapTy;
  typedef llvm::DenseMap<const ObjCIvarDecl*,
                         const ObjCPropertyDecl*> IvarToPropMapTy;

  /// Statement visitor, which walks the method body and flags the ivars
  /// referenced in it (either directly or via property).
  class MethodCrawler : public ConstStmtVisitor<MethodCrawler> {

    /// The set of Ivars which need to be invalidated.
    IvarSet &IVars;

    /// Property setter/getter to ivar mapping.
    MethToIvarMapTy &PropertyAccessorToIvarMap;

    // Property to ivar mapping.
    PropToIvarMapTy &PropertyToIvarMap;

  public:
    MethodCrawler(const ObjCInterfaceDecl *InID,
                  IvarSet &InIVars,
                  MethToIvarMapTy &InPropertyAccessorToIvarMap,
                  PropToIvarMapTy &InPropertyToIvarMap)
    : IVars(InIVars),
      PropertyAccessorToIvarMap(InPropertyAccessorToIvarMap),
      PropertyToIvarMap(InPropertyToIvarMap) {}

    void VisitStmt(const Stmt *S) { VisitChildren(S); }

    void VisitObjCIvarRefExpr(const ObjCIvarRefExpr *IvarRef);

    void VisitObjCMessageExpr(const ObjCMessageExpr *ME);

    void VisitObjCPropertyRefExpr(const ObjCPropertyRefExpr *PA);

    void VisitChildren(const Stmt *S) {
      for (Stmt::const_child_range I = S->children(); I; ++I)
        if (*I)
          this->Visit(*I);
    }
  };

  /// Check if the any of the methods inside the interface are annotated with
  /// the invalidation annotation.
  static bool containsInvalidationMethod(const ObjCContainerDecl *D);

  /// Check if ivar should be tracked and add to TrackedIvars if positive.
  /// Returns true if ivar should be tracked.
  static bool trackIvar(const ObjCIvarDecl *Iv, IvarSet &TrackedIvars);

  /// Given the property declaration, and the list of tracked ivars, finds
  /// the ivar backing the property when possible. Returns '0' when no such
  /// ivar could be found.
  static const ObjCIvarDecl *findPropertyBackingIvar(
      const ObjCPropertyDecl *Prop,
      const ObjCInterfaceDecl *InterfaceD,
      IvarSet &TrackedIvars);

public:
  void checkASTDecl(const ObjCMethodDecl *D, AnalysisManager& Mgr,
                    BugReporter &BR) const;

  // TODO: We are currently ignoring the ivars coming from class extensions.
};

bool isInvalidationMethod(const ObjCMethodDecl *M) {
  for (specific_attr_iterator<AnnotateAttr>
       AI = M->specific_attr_begin<AnnotateAttr>(),
       AE = M->specific_attr_end<AnnotateAttr>(); AI != AE; ++AI) {
    const AnnotateAttr *Ann = *AI;
    if (Ann->getAnnotation() == "objc_instance_variable_invalidator")
      return true;
  }
  return false;
}

bool IvarInvalidationChecker::containsInvalidationMethod (
    const ObjCContainerDecl *D) {

  // TODO: Cache the results.

  if (!D)
    return false;

  // Check all methods.
  for (ObjCContainerDecl::method_iterator
      I = D->meth_begin(),
      E = D->meth_end(); I != E; ++I) {
      const ObjCMethodDecl *MDI = *I;
      if (isInvalidationMethod(MDI))
        return true;
  }

  // If interface, check all parent protocols and super.
  // TODO: Visit all categories in case the invalidation method is declared in
  // a category.
  if (const ObjCInterfaceDecl *InterfaceD = dyn_cast<ObjCInterfaceDecl>(D)) {
    for (ObjCInterfaceDecl::protocol_iterator
        I = InterfaceD->protocol_begin(),
        E = InterfaceD->protocol_end(); I != E; ++I) {
      if (containsInvalidationMethod(*I))
        return true;
    }
    return containsInvalidationMethod(InterfaceD->getSuperClass());
  }

  // If protocol, check all parent protocols.
  if (const ObjCProtocolDecl *ProtD = dyn_cast<ObjCProtocolDecl>(D)) {
    for (ObjCInterfaceDecl::protocol_iterator
        I = ProtD->protocol_begin(),
        E = ProtD->protocol_end(); I != E; ++I) {
      if (containsInvalidationMethod(*I))
        return true;
    }
    return false;
  }

  llvm_unreachable("One of the casts above should have succeeded.");
}

bool IvarInvalidationChecker::trackIvar(const ObjCIvarDecl *Iv,
                                        IvarSet &TrackedIvars) {
  QualType IvQTy = Iv->getType();
  const ObjCObjectPointerType *IvTy = IvQTy->getAs<ObjCObjectPointerType>();
  if (!IvTy)
    return false;
  const ObjCInterfaceDecl *IvInterf = IvTy->getInterfaceDecl();
  if (containsInvalidationMethod(IvInterf)) {
    TrackedIvars[cast<ObjCIvarDecl>(Iv->getCanonicalDecl())] = false;
    return true;
  }
  return false;
}

const ObjCIvarDecl *IvarInvalidationChecker::findPropertyBackingIvar(
                        const ObjCPropertyDecl *Prop,
                        const ObjCInterfaceDecl *InterfaceD,
                        IvarSet &TrackedIvars) {
  const ObjCIvarDecl *IvarD = 0;

  // Lookup for the synthesized case.
  IvarD = Prop->getPropertyIvarDecl();
  if (IvarD) {
    if (TrackedIvars.count(IvarD)) {
      return IvarD;
    }
    // If the ivar is synthesized we still want to track it.
    if (trackIvar(IvarD, TrackedIvars))
      return IvarD;
  }

  // Lookup IVars named "_PropName"or "PropName" among the tracked Ivars.
  StringRef PropName = Prop->getIdentifier()->getName();
  for (IvarSet::const_iterator I = TrackedIvars.begin(),
                               E = TrackedIvars.end(); I != E; ++I) {
    const ObjCIvarDecl *Iv = I->first;
    StringRef IvarName = Iv->getName();

    if (IvarName == PropName)
      return Iv;

    SmallString<128> PropNameWithUnderscore;
    {
      llvm::raw_svector_ostream os(PropNameWithUnderscore);
      os << '_' << PropName;
    }
    if (IvarName == PropNameWithUnderscore.str())
      return Iv;
  }

  // Note, this is a possible source of false positives. We could look at the
  // getter implementation to find the ivar when its name is not derived from
  // the property name.
  return 0;
}

void IvarInvalidationChecker::checkASTDecl(const ObjCMethodDecl *D,
                                          AnalysisManager& Mgr,
                                          BugReporter &BR) const {
  // We are only interested in checking the cleanup methods.
  if (!D->hasBody() || !isInvalidationMethod(D))
    return;

  // Collect all ivars that need cleanup.
  IvarSet Ivars;
  const ObjCInterfaceDecl *InterfaceD = D->getClassInterface();
  for (ObjCInterfaceDecl::ivar_iterator
      II = InterfaceD->ivar_begin(),
      IE = InterfaceD->ivar_end(); II != IE; ++II) {
    const ObjCIvarDecl *Iv = *II;
    trackIvar(Iv, Ivars);
  }

  // Construct Property/Property Accessor to Ivar maps to assist checking if an
  // ivar which is backing a property has been reset.
  MethToIvarMapTy PropAccessorToIvarMap;
  PropToIvarMapTy PropertyToIvarMap;
  IvarToPropMapTy IvarToPopertyMap;
  for (ObjCInterfaceDecl::prop_iterator
      I = InterfaceD->prop_begin(),
      E = InterfaceD->prop_end(); I != E; ++I) {
    const ObjCPropertyDecl *PD = *I;

    const ObjCIvarDecl *ID = findPropertyBackingIvar(PD, InterfaceD, Ivars);
    if (!ID) {
      continue;
    }

    // Store the mappings.
    PD = cast<ObjCPropertyDecl>(PD->getCanonicalDecl());
    PropertyToIvarMap[PD] = ID;
    IvarToPopertyMap[ID] = PD;

    // Find the setter and the getter.
    const ObjCMethodDecl *SetterD = PD->getSetterMethodDecl();
    if (SetterD) {
      SetterD = cast<ObjCMethodDecl>(SetterD->getCanonicalDecl());
      PropAccessorToIvarMap[SetterD] = ID;
    }

    const ObjCMethodDecl *GetterD = PD->getGetterMethodDecl();
    if (GetterD) {
      GetterD = cast<ObjCMethodDecl>(GetterD->getCanonicalDecl());
      PropAccessorToIvarMap[GetterD] = ID;
    }
  }


  // Check which ivars have been accessed by the method.
  // We assume that if ivar was at least accessed, it was not forgotten.
  MethodCrawler(InterfaceD, Ivars,
                PropAccessorToIvarMap, PropertyToIvarMap).VisitStmt(D->getBody());

  // Warn on the ivars that were not accessed by the method.
  for (IvarSet::const_iterator I = Ivars.begin(), E = Ivars.end(); I != E; ++I){
    if (I->second == false) {
      const ObjCIvarDecl *IvarDecl = I->first;

      PathDiagnosticLocation IvarDecLocation =
          PathDiagnosticLocation::createEnd(D->getBody(), BR.getSourceManager(),
                                            Mgr.getAnalysisDeclContext(D));

      SmallString<128> sbuf;
      llvm::raw_svector_ostream os(sbuf);

      // Construct the warning message.
      if (IvarDecl->getSynthesize()) {
        const ObjCPropertyDecl *PD = IvarToPopertyMap[IvarDecl];
        assert(PD &&
               "Do we synthesize ivars for something other than properties?");
        os << "Property "<< PD->getName() << " needs to be invalidated";
      } else {
        os << "Instance variable "<< IvarDecl->getName()
             << " needs to be invalidated";
      }

      BR.EmitBasicReport(D,
          "Incomplete invalidation",
          categories::CoreFoundationObjectiveC, os.str(),
          IvarDecLocation);
    }
  }
}

/// Handle the case when an ivar is directly accessed.
void IvarInvalidationChecker::MethodCrawler::VisitObjCIvarRefExpr(
    const ObjCIvarRefExpr *IvarRef) {
  const Decl *D = IvarRef->getDecl();
  if (D)
    IVars[cast<ObjCIvarDecl>(D->getCanonicalDecl())] = true;
  VisitStmt(IvarRef);
}


/// Handle the case when the property backing ivar is set via a direct call
/// to the setter.
void IvarInvalidationChecker::MethodCrawler::VisitObjCMessageExpr(
    const ObjCMessageExpr *ME) {
  const ObjCMethodDecl *MD = ME->getMethodDecl();
  if (MD) {
    MD = cast<ObjCMethodDecl>(MD->getCanonicalDecl());
    IVars[PropertyAccessorToIvarMap[MD]] = true;
  }
  VisitStmt(ME);
}

/// Handle the case when the property backing ivar is set via the dot syntax.
void IvarInvalidationChecker::MethodCrawler::VisitObjCPropertyRefExpr(
    const ObjCPropertyRefExpr *PA) {

  if (PA->isExplicitProperty()) {
    const ObjCPropertyDecl *PD = PA->getExplicitProperty();
    if (PD) {
      PD = cast<ObjCPropertyDecl>(PD->getCanonicalDecl());
      IVars[PropertyToIvarMap[PD]] = true;
      VisitStmt(PA);
      return;
    }
  }

  if (PA->isImplicitProperty()) {
    const ObjCMethodDecl *MD = PA->getImplicitPropertySetter();
    if (MD) {
      MD = cast<ObjCMethodDecl>(MD->getCanonicalDecl());
      IVars[PropertyAccessorToIvarMap[MD]] = true;
      VisitStmt(PA);
      return;
    }
  }
  VisitStmt(PA);
}
}

// Register the checker.
void ento::registerIvarInvalidationChecker(CheckerManager &mgr) {
  mgr.registerChecker<IvarInvalidationChecker>();
}
