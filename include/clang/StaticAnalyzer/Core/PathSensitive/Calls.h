//===- Calls.h - Wrapper for all function and method calls --------*- C++ -*--//
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

#ifndef LLVM_CLANG_STATICANALYZER_PATHSENSITIVE_CALL
#define LLVM_CLANG_STATICANALYZER_PATHSENSITIVE_CALL

#include "clang/Basic/SourceManager.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/ExprCXX.h"
#include "clang/AST/ExprObjC.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/ProgramState.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/SVals.h"

namespace clang {
class ProgramPoint;
class ProgramPointTag;

namespace ento {

enum CallEventKind {
  CE_Function,
  CE_CXXMember,
  CE_CXXMemberOperator,
  CE_BEG_CXX_INSTANCE_CALLS = CE_CXXMember,
  CE_END_CXX_INSTANCE_CALLS = CE_CXXMemberOperator,
  CE_Block,
  CE_BEG_SIMPLE_CALLS = CE_Function,
  CE_END_SIMPLE_CALLS = CE_Block,
  CE_CXXConstructor,
  CE_CXXDestructor,
  CE_CXXAllocator,
  CE_BEG_FUNCTION_CALLS = CE_Function,
  CE_END_FUNCTION_CALLS = CE_CXXAllocator,
  CE_ObjCMessage,
  CE_ObjCPropertyAccess,
  CE_BEG_OBJC_CALLS = CE_ObjCMessage,
  CE_END_OBJC_CALLS = CE_ObjCPropertyAccess
};

/// \brief Represents an abstract call to a function or method along a
/// particular path.
class CallEvent {
public:
  typedef CallEventKind Kind;

protected:
  ProgramStateRef State;
  const LocationContext *LCtx;
  const Kind K;

  CallEvent(ProgramStateRef state, const LocationContext *lctx, Kind k)
    : State(state), LCtx(lctx), K(k) {}
  virtual ~CallEvent() {}

  /// \brief Get the value of arbitrary expressions at this point in the path.
  SVal getSVal(const Stmt *S) const {
    return State->getSVal(S, LCtx);
  }

  typedef SmallVectorImpl<const MemRegion *> RegionList;

  /// \brief Used to specify non-argument regions that will be invalidated as a
  /// result of this call.
  virtual void addExtraInvalidatedRegions(RegionList &Regions) const {}

  virtual QualType getDeclaredResultType() const { return QualType(); }

public:
  /// \brief Returns the declaration of the function or method that will be
  /// called. May be null.
  virtual const Decl *getDecl() const = 0;

  /// \brief Returns the definition of the function or method that will be
  /// called. May be null.
  ///
  /// This is used when deciding how to inline the call.
  ///
  /// \param IsDynamicDispatch True if the definition returned may not be the
  ///   definition that is actually invoked at runtime. Note that if we have
  ///   sufficient type information to devirtualize a dynamic method call,
  ///   we will (and \p IsDynamicDispatch will be set to \c false).
  virtual const Decl *getDefinition(bool &IsDynamicDispatch) const {
    IsDynamicDispatch = false;
    return getDecl();
  }

  /// \brief Returns the expression whose value will be the result of this call.
  /// May be null.
  virtual const Expr *getOriginExpr() const = 0;

  /// \brief Returns the number of arguments (explicit and implicit).
  ///
  /// Note that this may be greater than the number of parameters in the
  /// callee's declaration, and that it may include arguments not written in
  /// the source.
  virtual unsigned getNumArgs() const = 0;

  /// \brief Returns true if the callee is known to be from a system header.
  bool isInSystemHeader() const {
    const Decl *D = getDecl();
    if (!D)
      return false;

    SourceLocation Loc = D->getLocation();
    if (Loc.isValid()) {
      const SourceManager &SM =
        State->getStateManager().getContext().getSourceManager();
      return SM.isInSystemHeader(D->getLocation());
    }

    // Special case for implicitly-declared global operator new/delete.
    // These should be considered system functions.
    if (const FunctionDecl *FD = dyn_cast<FunctionDecl>(D))
      return FD->isOverloadedOperator() && FD->isImplicit() && FD->isGlobal();

    return false;
  }

  /// \brief Returns the kind of call this is.
  Kind getKind() const { return K; }

  /// \brief Returns a source range for the entire call, suitable for
  /// outputting in diagnostics.
  virtual SourceRange getSourceRange() const = 0;

  /// \brief Returns the value of a given argument at the time of the call.
  virtual SVal getArgSVal(unsigned Index) const;

  /// \brief Returns the expression associated with a given argument.
  /// May be null if this expression does not appear in the source.
  virtual const Expr *getArgExpr(unsigned Index) const {
    return 0;
  }

  /// \brief Returns the source range for errors associated with this argument.
  /// May be invalid if the argument is not written in the source.
  // FIXME: Is it better to return an invalid range or the range of the origin
  // expression?
  virtual SourceRange getArgSourceRange(unsigned Index) const;

  /// \brief Returns the result type, adjusted for references.
  QualType getResultType() const;

  /// \brief Returns the value of the implicit 'this' object, or UndefinedVal if
  /// this is not a C++ member function call.
  virtual SVal getCXXThisVal() const { return UndefinedVal(); }

  /// \brief Returns true if any of the arguments appear to represent callbacks.
  bool hasNonZeroCallbackArg() const;

  /// \brief Returns true if any of the arguments are known to escape to long-
  /// term storage, even if this method will not modify them.
  // NOTE: The exact semantics of this are still being defined!
  // We don't really want a list of hardcoded exceptions in the long run,
  // but we don't want duplicated lists of known APIs in the short term either.
  virtual bool argumentsMayEscape() const {
    return hasNonZeroCallbackArg();
  }

  /// \brief Returns an appropriate ProgramPoint for this call.
  ProgramPoint getProgramPoint(bool IsPreVisit = false,
                               const ProgramPointTag *Tag = 0) const;

  /// \brief Returns a new state with all argument regions invalidated.
  ///
  /// This accepts an alternate state in case some processing has already
  /// occurred.
  ProgramStateRef invalidateRegions(unsigned BlockCount,
                                    ProgramStateRef Orig = 0) const;

  /// \brief Returns true if this is a statement that can be considered for
  /// inlining.
  static bool mayBeInlined(const Stmt *S);

  // Iterator access to formal parameters and their types.
private:
  typedef std::const_mem_fun_t<QualType, ParmVarDecl> get_type_fun;
  
public:
  typedef const ParmVarDecl * const *param_iterator;

  /// Returns an iterator over the call's formal parameters.
  ///
  /// If UseDefinitionParams is set, this will return the parameter decls
  /// used in the callee's definition (suitable for inlining). Most of the
  /// time it is better to use the decl found by name lookup, which likely
  /// carries more annotations.
  ///
  /// Remember that the number of formal parameters may not match the number
  /// of arguments for all calls. However, the first parameter will always
  /// correspond with the argument value returned by \c getArgSVal(0).
  ///
  /// If the call has no accessible declaration (or definition, if
  /// \p UseDefinitionParams is set), \c param_begin() will be equal to
  /// \c param_end().
  virtual param_iterator param_begin(bool UseDefinitionParams = false) const = 0;
  /// \sa param_begin()
  virtual param_iterator param_end(bool UseDefinitionParams = false) const = 0;

  typedef llvm::mapped_iterator<param_iterator, get_type_fun>
    param_type_iterator;

  /// Returns an iterator over the types of the call's formal parameters.
  ///
  /// This uses the callee decl found by default name lookup rather than the
  /// definition because it represents a public interface, and probably has
  /// more annotations.
  param_type_iterator param_type_begin() const {
    return llvm::map_iterator(param_begin(),
                              get_type_fun(&ParmVarDecl::getType));
  }
  /// \sa param_type_begin()
  param_type_iterator param_type_end() const {
    return llvm::map_iterator(param_end(), get_type_fun(&ParmVarDecl::getType));
  }

  // For debugging purposes only
  virtual void dump(raw_ostream &Out) const;
  LLVM_ATTRIBUTE_USED void dump() const { dump(llvm::errs()); }

  static bool classof(const CallEvent *) { return true; }
};

/// \brief Represents a call to any sort of function that might have a
/// FunctionDecl.
class AnyFunctionCall : public CallEvent {
protected:
  AnyFunctionCall(ProgramStateRef St, const LocationContext *LCtx, Kind K)
    : CallEvent(St, LCtx, K) {}

  param_iterator param_begin(bool UseDefinitionParams = false) const;
  param_iterator param_end(bool UseDefinitionParams = false) const;

  QualType getDeclaredResultType() const;

public:
  virtual const FunctionDecl *getDecl() const = 0;

  const Decl *getDefinition(bool &IsDynamicDispatch) const {
    IsDynamicDispatch = false;
    const FunctionDecl *FD = getDecl();
    // Note that hasBody() will fill FD with the definition FunctionDecl.
    if (FD && FD->hasBody(FD))
      return FD;
    return 0;
  }

  bool argumentsMayEscape() const;

  static bool classof(const CallEvent *CA) {
    return CA->getKind() >= CE_BEG_FUNCTION_CALLS &&
           CA->getKind() <= CE_END_FUNCTION_CALLS;
  }
};

/// \brief Represents a call to a written as a CallExpr.
class SimpleCall : public AnyFunctionCall {
  const CallExpr *CE;

protected:
  SimpleCall(const CallExpr *ce, ProgramStateRef St,
             const LocationContext *LCtx, Kind K)
    : AnyFunctionCall(St, LCtx, K), CE(ce) {
  }

public:
  const CallExpr *getOriginExpr() const { return CE; }

  const FunctionDecl *getDecl() const;

  unsigned getNumArgs() const { return CE->getNumArgs(); }
  SourceRange getSourceRange() const { return CE->getSourceRange(); }
  
  const Expr *getArgExpr(unsigned Index) const {
    return CE->getArg(Index);
  }

  static bool classof(const CallEvent *CA) {
    return CA->getKind() >= CE_BEG_SIMPLE_CALLS &&
           CA->getKind() <= CE_END_SIMPLE_CALLS;
  }
};

/// \brief Represents a C function or static C++ member function call.
///
/// Example: \c fun()
class FunctionCall : public SimpleCall {
public:
  FunctionCall(const CallExpr *CE, ProgramStateRef St,
               const LocationContext *LCtx)
    : SimpleCall(CE, St, LCtx, CE_Function) {}

  static bool classof(const CallEvent *CA) {
    return CA->getKind() == CE_Function;
  }
};

/// \brief Represents a non-static C++ member function call, no matter how
/// it is written.
class CXXInstanceCall : public SimpleCall {
protected:
  void addExtraInvalidatedRegions(RegionList &Regions) const;

  CXXInstanceCall(const CallExpr *CE, ProgramStateRef St,
                  const LocationContext *LCtx, Kind K)
    : SimpleCall(CE, St, LCtx, K) {}

public:
  SVal getCXXThisVal() const = 0;

  const Decl *getDefinition(bool &IsDynamicDispatch) const;

  static bool classof(const CallEvent *CA) {
    return CA->getKind() >= CE_BEG_CXX_INSTANCE_CALLS &&
           CA->getKind() <= CE_END_CXX_INSTANCE_CALLS;
  }
};

/// \brief Represents a non-static C++ member function call.
///
/// Example: \c obj.fun()
class CXXMemberCall : public CXXInstanceCall {
public:
  CXXMemberCall(const CXXMemberCallExpr *CE, ProgramStateRef St,
                const LocationContext *LCtx)
    : CXXInstanceCall(CE, St, LCtx, CE_CXXMember) {}

  const CXXMemberCallExpr *getOriginExpr() const {
    return cast<CXXMemberCallExpr>(SimpleCall::getOriginExpr());
  }

  SVal getCXXThisVal() const;

  static bool classof(const CallEvent *CA) {
    return CA->getKind() == CE_CXXMember;
  }
};

/// \brief Represents a C++ overloaded operator call where the operator is
/// implemented as a non-static member function.
///
/// Example: <tt>iter + 1</tt>
class CXXMemberOperatorCall : public CXXInstanceCall {
public:
  CXXMemberOperatorCall(const CXXOperatorCallExpr *CE, ProgramStateRef St,
                        const LocationContext *LCtx)
    : CXXInstanceCall(CE, St, LCtx, CE_CXXMemberOperator) {}

  const CXXOperatorCallExpr *getOriginExpr() const {
    return cast<CXXOperatorCallExpr>(SimpleCall::getOriginExpr());
  }

  unsigned getNumArgs() const { return getOriginExpr()->getNumArgs() - 1; }
  const Expr *getArgExpr(unsigned Index) const {
    return getOriginExpr()->getArg(Index + 1);
  }

  SVal getCXXThisVal() const;

  static bool classof(const CallEvent *CA) {
    return CA->getKind() == CE_CXXMemberOperator;
  }
};

/// \brief Represents a call to a block.
///
/// Example: <tt>^{ /* ... */ }()</tt>
class BlockCall : public SimpleCall {
protected:
  void addExtraInvalidatedRegions(RegionList &Regions) const;

  param_iterator param_begin(bool UseDefinitionParams = false) const;
  param_iterator param_end(bool UseDefinitionParams = false) const;

  QualType getDeclaredResultType() const;

public:
  BlockCall(const CallExpr *CE, ProgramStateRef St,
            const LocationContext *LCtx)
    : SimpleCall(CE, St, LCtx, CE_Block) {}

  /// \brief Returns the region associated with this instance of the block.
  ///
  /// This may be NULL if the block's origin is unknown.
  const BlockDataRegion *getBlockRegion() const;

  /// \brief Gets the declaration of the block.
  ///
  /// This is not an override of getDecl() because AnyFunctionCall has already
  /// assumed that it's a FunctionDecl.
  const BlockDecl *getBlockDecl() const {
    const BlockDataRegion *BR = getBlockRegion();
    if (!BR)
      return 0;
    return BR->getDecl();
  }

  const Decl *getDefinition(bool &IsDynamicDispatch) const {
    IsDynamicDispatch = false;
    return getBlockDecl();
  }

  static bool classof(const CallEvent *CA) {
    return CA->getKind() == CE_Block;
  }
};

/// \brief Represents a call to a C++ constructor.
///
/// Example: \c T(1)
class CXXConstructorCall : public AnyFunctionCall {
  const CXXConstructExpr *CE;
  const MemRegion *Target;

protected:
  void addExtraInvalidatedRegions(RegionList &Regions) const;

public:
  CXXConstructorCall(const CXXConstructExpr *ce, ProgramStateRef St,
                     const LocationContext *LCtx)
    : AnyFunctionCall(St, LCtx, CE_CXXConstructor), CE(ce), Target(0) {}
  CXXConstructorCall(const CXXConstructExpr *ce, const MemRegion *target,
                     ProgramStateRef St, const LocationContext *LCtx)
    : AnyFunctionCall(St, LCtx, CE_CXXConstructor), CE(ce), Target(target) {}

  const CXXConstructExpr *getOriginExpr() const { return CE; }
  SourceRange getSourceRange() const { return CE->getSourceRange(); }

  const CXXConstructorDecl *getDecl() const {
    return CE->getConstructor();
  }

  unsigned getNumArgs() const { return CE->getNumArgs(); }

  const Expr *getArgExpr(unsigned Index) const {
    return CE->getArg(Index);
  }

  SVal getCXXThisVal() const;

  static bool classof(const CallEvent *CA) {
    return CA->getKind() == CE_CXXConstructor;
  }
};

/// \brief Represents an implicit call to a C++ destructor.
///
/// This can occur at the end of a scope (for automatic objects), at the end
/// of a full-expression (for temporaries), or as part of a delete.
class CXXDestructorCall : public AnyFunctionCall {
  const CXXDestructorDecl *DD;
  const MemRegion *Target;
  SourceLocation Loc;

protected:
  void addExtraInvalidatedRegions(RegionList &Regions) const;

public:
  CXXDestructorCall(const CXXDestructorDecl *dd, const Stmt *Trigger,
                    const MemRegion *target, ProgramStateRef St,
                    const LocationContext *LCtx)
    : AnyFunctionCall(St, LCtx, CE_CXXDestructor), DD(dd), Target(target),
      Loc(Trigger->getLocEnd()) {}

  const Expr *getOriginExpr() const { return 0; }
  SourceRange getSourceRange() const { return Loc; }

  const CXXDestructorDecl *getDecl() const { return DD; }
  unsigned getNumArgs() const { return 0; }

  SVal getCXXThisVal() const;
  const Decl *getDefinition(bool &IsDynamicDispatch) const;

  static bool classof(const CallEvent *CA) {
    return CA->getKind() == CE_CXXDestructor;
  }
};

/// \brief Represents the memory allocation call in a C++ new-expression.
///
/// This is a call to "operator new".
class CXXAllocatorCall : public AnyFunctionCall {
  const CXXNewExpr *E;

public:
  CXXAllocatorCall(const CXXNewExpr *e, ProgramStateRef St,
                   const LocationContext *LCtx)
    : AnyFunctionCall(St, LCtx, CE_CXXAllocator), E(e) {}

  const CXXNewExpr *getOriginExpr() const { return E; }
  SourceRange getSourceRange() const { return E->getSourceRange(); }

  const FunctionDecl *getDecl() const {
    return E->getOperatorNew();
  }

  unsigned getNumArgs() const { return E->getNumPlacementArgs() + 1; }

  const Expr *getArgExpr(unsigned Index) const {
    // The first argument of an allocator call is the size of the allocation.
    if (Index == 0)
      return 0;
    return E->getPlacementArg(Index - 1);
  }

  static bool classof(const CallEvent *CE) {
    return CE->getKind() == CE_CXXAllocator;
  }
};

/// \brief Represents any expression that calls an Objective-C method.
class ObjCMethodCall : public CallEvent {
  const ObjCMessageExpr *Msg;

protected:
  ObjCMethodCall(const ObjCMessageExpr *msg, ProgramStateRef St,
                 const LocationContext *LCtx, Kind K)
    : CallEvent(St, LCtx, K), Msg(msg) {}

  void addExtraInvalidatedRegions(RegionList &Regions) const;

  param_iterator param_begin(bool UseDefinitionParams = false) const;
  param_iterator param_end(bool UseDefinitionParams = false) const;

  QualType getDeclaredResultType() const;

public:
  Selector getSelector() const { return Msg->getSelector(); }
  bool isInstanceMessage() const { return Msg->isInstanceMessage(); }
  ObjCMethodFamily getMethodFamily() const { return Msg->getMethodFamily(); }

  const ObjCMethodDecl *getDecl() const { return Msg->getMethodDecl(); }
  SourceRange getSourceRange() const { return Msg->getSourceRange(); }
  unsigned getNumArgs() const { return Msg->getNumArgs(); }
  const Expr *getArgExpr(unsigned Index) const {
    return Msg->getArg(Index);
  }

  const ObjCMessageExpr *getOriginExpr() const { return Msg; }

  /// \brief Returns the value of the receiver at the time of this call.
  SVal getReceiverSVal() const;

  /// \brief Returns the expression for the receiver of this message if it is
  /// an instance message.
  ///
  /// Returns NULL otherwise.
  /// \sa ObjCMessageExpr::getInstanceReceiver()
  const Expr *getInstanceReceiverExpr() const {
    return Msg->getInstanceReceiver();
  }

  /// \brief Get the interface for the receiver.
  ///
  /// This works whether this is an instance message or a class message.
  /// However, it currently just uses the static type of the receiver.
  const ObjCInterfaceDecl *getReceiverInterface() const {
    return Msg->getReceiverInterface();
  }

  SourceRange getReceiverSourceRange() const {
    return Msg->getReceiverRange();
  }

  const Decl *getDefinition(bool &IsDynamicDispatch) const {
    IsDynamicDispatch = true;
    
    const ObjCMethodDecl *MD = getDecl();
    for (Decl::redecl_iterator I = MD->redecls_begin(), E = MD->redecls_end();
         I != E; ++I) {
      if (cast<ObjCMethodDecl>(*I)->isThisDeclarationADefinition())
        return *I;
    }
    return 0;
  }

  static bool classof(const CallEvent *CA) {
    return CA->getKind() >= CE_BEG_OBJC_CALLS &&
           CA->getKind() <= CE_END_OBJC_CALLS;
  }
};

/// \brief Represents an explicit message send to an Objective-C object.
///
/// Example: [obj descriptionWithLocale:locale];
class ObjCMessageSend : public ObjCMethodCall {
public:
  ObjCMessageSend(const ObjCMessageExpr *Msg, ProgramStateRef St,
                  const LocationContext *LCtx)
    : ObjCMethodCall(Msg, St, LCtx, CE_ObjCMessage) {}

  static bool classof(const CallEvent *CA) {
    return CA->getKind() == CE_ObjCMessage;
  }
};

/// \brief Represents an Objective-C property getter or setter invocation.
///
/// Example: obj.prop += 1;
class ObjCPropertyAccess : public ObjCMethodCall {
  const ObjCPropertyRefExpr *PropE;
  SourceRange EntireRange;

public:
  ObjCPropertyAccess(const ObjCPropertyRefExpr *pe, SourceRange range,
                     const ObjCMessageExpr *Msg, const ProgramStateRef St,
                     const LocationContext *LCtx)
    : ObjCMethodCall(Msg, St, LCtx, CE_ObjCPropertyAccess), PropE(pe),
      EntireRange(range)
    {}

  /// \brief Returns true if this property access is calling the setter method.
  bool isSetter() const {
    return getNumArgs() > 0;
  }

  SourceRange getSourceRange() const {
    return EntireRange;
  }

  /// \brief Return the property reference part of this access.
  ///
  /// In the expression "obj.prop += 1", the property reference expression is
  /// "obj.prop".
  const ObjCPropertyRefExpr *getPropertyExpr() const {
    return PropE;
  }

  static bool classof(const CallEvent *CA) {
    return CA->getKind() == CE_ObjCPropertyAccess;
  }
};

} // end namespace ento
} // end namespace clang

#endif
