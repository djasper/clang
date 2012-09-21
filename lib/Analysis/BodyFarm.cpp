//== BodyFarm.cpp  - Factory for conjuring up fake bodies ----------*- C++ -*-//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// BodyFarm is a factory for creating faux implementations for functions/methods
// for analysis purposes.
//
//===----------------------------------------------------------------------===//

#include "llvm/ADT/StringSwitch.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Expr.h"
#include "clang/AST/Decl.h"
#include "BodyFarm.h"

using namespace clang;

//===----------------------------------------------------------------------===//
// Helper creation functions for constructing faux ASTs.
//===----------------------------------------------------------------------===//

static bool isDispatchBlock(QualType Ty) {
  // Is it a block pointer?
  const BlockPointerType *BPT = Ty->getAs<BlockPointerType>();
  if (!BPT)
    return false;

  // Check if the block pointer type takes no arguments and
  // returns void.
  const FunctionProtoType *FT =
  BPT->getPointeeType()->getAs<FunctionProtoType>();
  if (!FT || !FT->getResultType()->isVoidType()  ||
      FT->getNumArgs() != 0)
    return false;

  return true;
}

//===----------------------------------------------------------------------===//
// Creation functions for faux ASTs.
//===----------------------------------------------------------------------===//

typedef Stmt *(*FunctionFarmer)(ASTContext &C, const FunctionDecl *D);

/// Create a fake body for dispatch_once.
static Stmt *create_dispatch_once(ASTContext &C, const FunctionDecl *D) {
  // Check if we have at least two parameters.
  if (D->param_size() != 2)
    return 0;

  // Check if the first parameter is a pointer to integer type.
  const ParmVarDecl *Predicate = D->getParamDecl(0);
  QualType PredicateQPtrTy = Predicate->getType();
  const PointerType *PredicatePtrTy = PredicateQPtrTy->getAs<PointerType>();
  if (!PredicatePtrTy)
    return 0;
  QualType PredicateTy = PredicatePtrTy->getPointeeType();
  if (!PredicateTy->isIntegerType())
    return 0;
  
  // Check if the second parameter is the proper block type.
  const ParmVarDecl *Block = D->getParamDecl(1);
  QualType Ty = Block->getType();
  if (!isDispatchBlock(Ty))
    return 0;
  
  // Everything checks out.  Create a fakse body that checks the predicate,
  // sets it, and calls the block.  Basically, an AST dump of:
  //
  // void dispatch_once(dispatch_once_t *predicate, dispatch_block_t block) {
  //  if (!*predicate) {
  //    *predicate = 1;
  //    block();
  //  }
  // }
  
  // (1) Create the call.
  DeclRefExpr *DR = DeclRefExpr::CreateEmpty(C, false, false, false, false);
  DR->setDecl(const_cast<ParmVarDecl*>(Block));
  DR->setType(Ty);
  DR->setValueKind(VK_LValue);
  ImplicitCastExpr *ICE = ImplicitCastExpr::Create(C, Ty, CK_LValueToRValue,
                                                   DR, 0, VK_RValue);
  CallExpr *CE = new (C) CallExpr(C, ICE, ArrayRef<Expr*>(), C.VoidTy,
                                  VK_RValue, SourceLocation());

  // (2) Create the assignment to the predicate.
  IntegerLiteral *IL =
    IntegerLiteral::Create(C, llvm::APInt(C.getTypeSize(C.IntTy), (uint64_t) 1),
                           C.IntTy, SourceLocation());
  ICE = ImplicitCastExpr::Create(C, PredicateTy, CK_IntegralCast, IL, 0,
                                 VK_RValue);
  DR = DeclRefExpr::CreateEmpty(C, false, false, false, false);
  DR->setDecl(const_cast<ParmVarDecl*>(Predicate));
  DR->setType(PredicateQPtrTy);
  DR->setValueKind(VK_LValue);
  ImplicitCastExpr *LValToRval =
    ImplicitCastExpr::Create(C, PredicateQPtrTy, CK_LValueToRValue, DR,
                             0, VK_RValue);
  UnaryOperator *UO = new (C) UnaryOperator(LValToRval, UO_Deref, PredicateTy,
                                            VK_LValue, OK_Ordinary,
                                            SourceLocation());
  BinaryOperator *B = new (C) BinaryOperator(UO, ICE, BO_Assign,
                                             PredicateTy, VK_RValue,
                                             OK_Ordinary,
                                             SourceLocation());
  // (3) Create the compound statement.
  Stmt *Stmts[2];
  Stmts[0] = B;
  Stmts[1] = CE;  
  CompoundStmt *CS = new (C) CompoundStmt(C, Stmts, 2, SourceLocation(),
                                          SourceLocation());
  
  // (4) Create the 'if' condition.
  DR = DeclRefExpr::CreateEmpty(C, false, false, false, false);
  DR->setDecl(const_cast<ParmVarDecl*>(Predicate));
  DR->setType(PredicateQPtrTy);
  DR->setValueKind(VK_LValue);
  LValToRval = ImplicitCastExpr::Create(C, PredicateQPtrTy, CK_LValueToRValue,
                                        DR, 0, VK_RValue);
  UO = new (C) UnaryOperator(LValToRval, UO_Deref, PredicateTy,
                             VK_LValue, OK_Ordinary,
                             SourceLocation());
  LValToRval = ImplicitCastExpr::Create(C, PredicateTy, CK_LValueToRValue,
                                        UO, 0, VK_RValue);
  UO = new (C) UnaryOperator(LValToRval, UO_LNot, C.IntTy,
                             VK_RValue, OK_Ordinary, SourceLocation());
  
  // (5) Create the 'if' statement.
  IfStmt *If = new (C) IfStmt(C, SourceLocation(), 0, UO, CS);
  return If;
}

  

/// Create a fake body for dispatch_sync.
static Stmt *create_dispatch_sync(ASTContext &C, const FunctionDecl *D) {
  // Check if we have at least two parameters.
  if (D->param_size() != 2)
    return 0;
  
  // Check if the second parameter is a block.
  const ParmVarDecl *PV = D->getParamDecl(1);
  QualType Ty = PV->getType();
  if (!isDispatchBlock(Ty))
    return 0;

  // Everything checks out.  Create a fake body that just calls the block.
  // This is basically just an AST dump of:
  //
  // void dispatch_sync(dispatch_queue_t queue, void (^block)(void)) {
  //   block();
  // }
  //
  DeclRefExpr *DR = DeclRefExpr::CreateEmpty(C, false, false, false, false);
  DR->setDecl(const_cast<ParmVarDecl*>(PV));
  DR->setType(Ty);
  DR->setValueKind(VK_LValue);
  ImplicitCastExpr *ICE = ImplicitCastExpr::Create(C, Ty, CK_LValueToRValue,
                                                   DR, 0, VK_RValue);
  CallExpr *CE = new (C) CallExpr(C, ICE, ArrayRef<Expr*>(), C.VoidTy,
                                  VK_RValue, SourceLocation());
  return CE;
}

Stmt *BodyFarm::getBody(const FunctionDecl *D) {
  D = D->getCanonicalDecl();
  
  llvm::Optional<Stmt *> &Val = Bodies[D];
  if (Val.hasValue())
    return Val.getValue();
  
  Val = 0;
  
  if (D->getIdentifier() == 0)
    return 0;

  StringRef Name = D->getName();
  if (Name.empty())
    return 0;
  
  FunctionFarmer FF =
    llvm::StringSwitch<FunctionFarmer>(Name)
      .Case("dispatch_sync", create_dispatch_sync)
      .Case("dispatch_once", create_dispatch_once)
      .Default(NULL);
  
  if (FF) {
    Val = FF(C, D);
  }
  
  return Val.getValue();
}

