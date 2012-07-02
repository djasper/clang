//===- Calls.cpp - Wrapper for all function and method calls ------*- C++ -*--//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
/// \file This file defines CallEvent and its subclasses, which represent path-
/// sensitive instances of different kinds of function and method calls
/// (C, C++, and Objective-C).
//
//===----------------------------------------------------------------------===//

#include "clang/StaticAnalyzer/Core/PathSensitive/Calls.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/ObjCMessage.h"
#include "llvm/ADT/SmallSet.h"

using namespace clang;
using namespace ento;

SVal CallEvent::getArgSVal(unsigned Index) const {
  const Expr *ArgE = getArgExpr(Index);
  if (!ArgE)
    return UnknownVal();
  return getSVal(ArgE);
}

SourceRange CallEvent::getArgSourceRange(unsigned Index) const {
  const Expr *ArgE = getArgExpr(Index);
  if (!ArgE)
    return SourceRange();
  return ArgE->getSourceRange();
}

QualType CallEvent::getResultType() const {
  QualType ResultTy = getDeclaredResultType();

  if (const Expr *E = getOriginExpr()) {
    if (ResultTy.isNull())
      ResultTy = E->getType();

    // FIXME: This is copied from CallOrObjCMessage, but it seems suspicious.
    if (E->isGLValue()) {
      ASTContext &Ctx = State->getStateManager().getContext();
      ResultTy = Ctx.getPointerType(ResultTy);
    }
  }

  return ResultTy;
}

static bool isCallbackArg(SVal V, QualType T) {
  // If the parameter is 0, it's harmless.
  if (V.isZeroConstant())
    return false;

  // If a parameter is a block or a callback, assume it can modify pointer.
  if (T->isBlockPointerType() ||
      T->isFunctionPointerType() ||
      T->isObjCSelType())
    return true;

  // Check if a callback is passed inside a struct (for both, struct passed by
  // reference and by value). Dig just one level into the struct for now.

  if (isa<PointerType>(T) || isa<ReferenceType>(T))
    T = T->getPointeeType();

  if (const RecordType *RT = T->getAsStructureType()) {
    const RecordDecl *RD = RT->getDecl();
    for (RecordDecl::field_iterator I = RD->field_begin(), E = RD->field_end();
         I != E; ++I) {
      QualType FieldT = I->getType();
      if (FieldT->isBlockPointerType() || FieldT->isFunctionPointerType())
        return true;
    }
  }

  return false;
}

bool CallEvent::hasNonZeroCallbackArg() const {
  unsigned NumOfArgs = getNumArgs();

  // If calling using a function pointer, assume the function does not
  // have a callback. TODO: We could check the types of the arguments here.
  if (!getDecl())
    return false;

  unsigned Idx = 0;
  for (CallEvent::param_type_iterator I = param_type_begin(),
                                       E = param_type_end();
       I != E && Idx < NumOfArgs; ++I, ++Idx) {
    if (NumOfArgs <= Idx)
      break;

    if (isCallbackArg(getArgSVal(Idx), *I))
      return true;
  }
  
  return false;
}

/// \brief Returns true if a type is a pointer-to-const or reference-to-const
/// with no further indirection.
static bool isPointerToConst(QualType Ty) {
  QualType PointeeTy = Ty->getPointeeType();
  if (PointeeTy == QualType())
    return false;
  if (!PointeeTy.isConstQualified())
    return false;
  if (PointeeTy->isAnyPointerType())
    return false;
  return true;
}

// Try to retrieve the function declaration and find the function parameter
// types which are pointers/references to a non-pointer const.
// We do not invalidate the corresponding argument regions.
static void findPtrToConstParams(llvm::SmallSet<unsigned, 1> &PreserveArgs,
                                 const CallEvent &Call) {
  const Decl *CallDecl = Call.getDecl();
  if (!CallDecl)
    return;

  if (Call.hasNonZeroCallbackArg())
    return;

  if (const FunctionDecl *FDecl = dyn_cast<FunctionDecl>(CallDecl)) {
    const IdentifierInfo *II = FDecl->getIdentifier();

    // List the cases, where the region should be invalidated even if the
    // argument is const.
    // FIXME: This is conflating invalidating /contents/ and invalidating
    // /metadata/. Now that we pass the CallEvent to the checkers, they
    // should probably be doing this work themselves.
    if (II) {
      StringRef FName = II->getName();
      //  - 'int pthread_setspecific(ptheread_key k, const void *)' stores a
      // value into thread local storage. The value can later be retrieved with
      // 'void *ptheread_getspecific(pthread_key)'. So even thought the
      // parameter is 'const void *', the region escapes through the call.
      //  - funopen - sets a buffer for future IO calls.
      //  - ObjC functions that end with "NoCopy" can free memory, of the passed
      // in buffer.
      // - Many CF containers allow objects to escape through custom
      // allocators/deallocators upon container construction.
      // - NSXXInsertXX, for example NSMapInsertIfAbsent, since they can
      // be deallocated by NSMapRemove.
      // - Any call that has a callback as one of the arguments.
      if (FName == "pthread_setspecific" ||
          FName == "funopen" ||
          FName.endswith("NoCopy") ||
          (FName.startswith("NS") &&
           (FName.find("Insert") != StringRef::npos)) ||
          CallOrObjCMessage::isCFCGAllowingEscape(FName))
        return;
    }
  }

  unsigned Idx = 0;
  for (CallEvent::param_type_iterator I = Call.param_type_begin(),
                                       E = Call.param_type_end();
       I != E; ++I, ++Idx) {
    if (isPointerToConst(*I))
      PreserveArgs.insert(Idx);
  }
}

ProgramStateRef CallEvent::invalidateRegions(unsigned BlockCount,
                                              ProgramStateRef Orig) const {
  ProgramStateRef Result = (Orig ? Orig : State);

  SmallVector<const MemRegion *, 8> RegionsToInvalidate;
  addExtraInvalidatedRegions(RegionsToInvalidate);

  // Indexes of arguments whose values will be preserved by the call.
  llvm::SmallSet<unsigned, 1> PreserveArgs;
  findPtrToConstParams(PreserveArgs, *this);

  for (unsigned Idx = 0, Count = getNumArgs(); Idx != Count; ++Idx) {
    if (PreserveArgs.count(Idx))
      continue;

    SVal V = getArgSVal(Idx);

    // If we are passing a location wrapped as an integer, unwrap it and
    // invalidate the values referred by the location.
    if (nonloc::LocAsInteger *Wrapped = dyn_cast<nonloc::LocAsInteger>(&V))
      V = Wrapped->getLoc();
    else if (!isa<Loc>(V))
      continue;

    if (const MemRegion *R = V.getAsRegion()) {
      // Invalidate the value of the variable passed by reference.

      // Are we dealing with an ElementRegion?  If the element type is
      // a basic integer type (e.g., char, int) and the underlying region
      // is a variable region then strip off the ElementRegion.
      // FIXME: We really need to think about this for the general case
      //   as sometimes we are reasoning about arrays and other times
      //   about (char*), etc., is just a form of passing raw bytes.
      //   e.g., void *p = alloca(); foo((char*)p);
      if (const ElementRegion *ER = dyn_cast<ElementRegion>(R)) {
        // Checking for 'integral type' is probably too promiscuous, but
        // we'll leave it in for now until we have a systematic way of
        // handling all of these cases.  Eventually we need to come up
        // with an interface to StoreManager so that this logic can be
        // appropriately delegated to the respective StoreManagers while
        // still allowing us to do checker-specific logic (e.g.,
        // invalidating reference counts), probably via callbacks.
        if (ER->getElementType()->isIntegralOrEnumerationType()) {
          const MemRegion *superReg = ER->getSuperRegion();
          if (isa<VarRegion>(superReg) || isa<FieldRegion>(superReg) ||
              isa<ObjCIvarRegion>(superReg))
            R = cast<TypedRegion>(superReg);
        }
        // FIXME: What about layers of ElementRegions?
      }

      // Mark this region for invalidation.  We batch invalidate regions
      // below for efficiency.
      RegionsToInvalidate.push_back(R);
    }
  }

  // Invalidate designated regions using the batch invalidation API.
  // NOTE: Even if RegionsToInvalidate is empty, we may still invalidate
  //  global variables.
  return Result->invalidateRegions(RegionsToInvalidate, getOriginExpr(),
                                   BlockCount, LCtx, /*Symbols=*/0, this);
}

CallEvent::param_iterator AnyFunctionCall::param_begin() const {
  const FunctionDecl *D = getDecl();
  if (!D)
    return 0;

  return D->param_begin();
}

CallEvent::param_iterator AnyFunctionCall::param_end() const {
  const FunctionDecl *D = getDecl();
  if (!D)
    return 0;

  return D->param_end();
}

QualType AnyFunctionCall::getDeclaredResultType() const {
  const FunctionDecl *D = getDecl();
  if (!D)
    return QualType();

  return D->getResultType();
}

const FunctionDecl *SimpleCall::getDecl() const {
  const FunctionDecl *D = CE->getDirectCallee();
  if (D)
    return D;

  return getSVal(CE->getCallee()).getAsFunctionDecl();
}

void CXXMemberCall::addExtraInvalidatedRegions(RegionList &Regions) const {
  const Expr *Base = getOriginExpr()->getImplicitObjectArgument();

  // FIXME: Will eventually need to cope with member pointers.  This is
  // a limitation in getImplicitObjectArgument().
  if (!Base)
    return;
    
  if (const MemRegion *R = getSVal(Base).getAsRegion())
    Regions.push_back(R);
}

const BlockDataRegion *BlockCall::getBlockRegion() const {
  const Expr *Callee = getOriginExpr()->getCallee();
  const MemRegion *DataReg = getSVal(Callee).getAsRegion();

  return cast<BlockDataRegion>(DataReg);
}

CallEvent::param_iterator BlockCall::param_begin() const {
  return getBlockRegion()->getDecl()->param_begin();
}

CallEvent::param_iterator BlockCall::param_end() const {
  return getBlockRegion()->getDecl()->param_end();
}

void BlockCall::addExtraInvalidatedRegions(RegionList &Regions) const {
  Regions.push_back(getBlockRegion());
}

QualType BlockCall::getDeclaredResultType() const {
  QualType BlockTy = getBlockRegion()->getCodeRegion()->getLocationType();
  return cast<FunctionType>(BlockTy->getPointeeType())->getResultType();
}

void CXXConstructorCall::addExtraInvalidatedRegions(RegionList &Regions) const {
  if (Target)
    Regions.push_back(Target);
}

CallEvent::param_iterator ObjCMessageInvocation::param_begin() const {
  const ObjCMethodDecl *D = getDecl();
  if (!D)
    return 0;

  return D->param_begin();
}

CallEvent::param_iterator ObjCMessageInvocation::param_end() const {
  const ObjCMethodDecl *D = getDecl();
  if (!D)
    return 0;

  return D->param_end();
}

void
ObjCMessageInvocation::addExtraInvalidatedRegions(RegionList &Regions) const {
  if (const MemRegion *R = getReceiverSVal().getAsRegion())
    Regions.push_back(R);
}

QualType ObjCMessageInvocation::getDeclaredResultType() const {
  const ObjCMethodDecl *D = getDecl();
  if (!D)
    return QualType();

  return D->getResultType();
}

SVal ObjCMessageInvocation::getReceiverSVal() const {
  // FIXME: Is this the best way to handle class receivers?
  if (!isInstanceMessage())
    return UnknownVal();
    
  const Expr *Base = Msg.getInstanceReceiver();
  if (Base)
    return getSVal(Base);

  // An instance message with no expression means we are sending to super.
  // In this case the object reference is the same as 'self'.
  const ImplicitParamDecl *SelfDecl = LCtx->getSelfDecl();
  assert(SelfDecl && "No message receiver Expr, but not in an ObjC method");
  return loc::MemRegionVal(State->getRegion(SelfDecl, LCtx));
}
