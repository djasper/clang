//===--- CommentSema.cpp - Doxygen comment semantic analysis --------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "clang/AST/CommentSema.h"
#include "clang/AST/CommentDiagnostic.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclObjC.h"
#include "clang/Basic/SourceManager.h"
#include "llvm/ADT/StringSwitch.h"

namespace clang {
namespace comments {

Sema::Sema(llvm::BumpPtrAllocator &Allocator, const SourceManager &SourceMgr,
           DiagnosticsEngine &Diags) :
    Allocator(Allocator), SourceMgr(SourceMgr), Diags(Diags), ThisDecl(NULL) {
}

void Sema::setDecl(const Decl *D) {
  ThisDecl = D;
}

ParagraphComment *Sema::actOnParagraphComment(
                              ArrayRef<InlineContentComment *> Content) {
  return new (Allocator) ParagraphComment(Content);
}

BlockCommandComment *Sema::actOnBlockCommandStart(SourceLocation LocBegin,
                                                  SourceLocation LocEnd,
                                                  StringRef Name) {
  return new (Allocator) BlockCommandComment(LocBegin, LocEnd, Name);
}

BlockCommandComment *Sema::actOnBlockCommandArgs(
                              BlockCommandComment *Command,
                              ArrayRef<BlockCommandComment::Argument> Args) {
  Command->setArgs(Args);
  return Command;
}

BlockCommandComment *Sema::actOnBlockCommandFinish(
                              BlockCommandComment *Command,
                              ParagraphComment *Paragraph) {
  Command->setParagraph(Paragraph);
  checkBlockCommandEmptyParagraph(Command);
  return Command;
}

ParamCommandComment *Sema::actOnParamCommandStart(SourceLocation LocBegin,
                                                  SourceLocation LocEnd,
                                                  StringRef Name) {
  ParamCommandComment *Command =
      new (Allocator) ParamCommandComment(LocBegin, LocEnd, Name);

  if (!ThisDecl ||
      !(isa<FunctionDecl>(ThisDecl) || isa<ObjCMethodDecl>(ThisDecl)))
    Diag(Command->getLocation(),
         diag::warn_doc_param_not_attached_to_a_function_decl)
      << Command->getCommandNameRange();

  return Command;
}

ParamCommandComment *Sema::actOnParamCommandDirectionArg(
                                                ParamCommandComment *Command,
                                                SourceLocation ArgLocBegin,
                                                SourceLocation ArgLocEnd,
                                                StringRef Arg) {
  ParamCommandComment::PassDirection Direction;
  std::string ArgLower = Arg.lower();
  // TODO: optimize: lower Name first (need an API in SmallString for that),
  // after that StringSwitch.
  if (ArgLower == "[in]")
    Direction = ParamCommandComment::In;
  else if (ArgLower == "[out]")
    Direction = ParamCommandComment::Out;
  else if (ArgLower == "[in,out]" || ArgLower == "[out,in]")
    Direction = ParamCommandComment::InOut;
  else {
    // Remove spaces.
    std::string::iterator O = ArgLower.begin();
    for (std::string::iterator I = ArgLower.begin(), E = ArgLower.end();
         I != E; ++I) {
      const char C = *I;
      if (C != ' ' && C != '\n' && C != '\r' &&
          C != '\t' && C != '\v' && C != '\f')
        *O++ = C;
    }
    ArgLower.resize(O - ArgLower.begin());

    bool RemovingWhitespaceHelped = false;
    if (ArgLower == "[in]") {
      Direction = ParamCommandComment::In;
      RemovingWhitespaceHelped = true;
    } else if (ArgLower == "[out]") {
      Direction = ParamCommandComment::Out;
      RemovingWhitespaceHelped = true;
    } else if (ArgLower == "[in,out]" || ArgLower == "[out,in]") {
      Direction = ParamCommandComment::InOut;
      RemovingWhitespaceHelped = true;
    } else {
      Direction = ParamCommandComment::In;
      RemovingWhitespaceHelped = false;
    }

    SourceRange ArgRange(ArgLocBegin, ArgLocEnd);
    if (RemovingWhitespaceHelped)
      Diag(ArgLocBegin, diag::warn_doc_param_spaces_in_direction)
        << ArgRange
        << FixItHint::CreateReplacement(
                          ArgRange,
                          ParamCommandComment::getDirectionAsString(Direction));
    else
      Diag(ArgLocBegin, diag::warn_doc_param_invalid_direction)
        << ArgRange;
  }
  Command->setDirection(Direction, /* Explicit = */ true);
  return Command;
}

ParamCommandComment *Sema::actOnParamCommandParamNameArg(
                                                ParamCommandComment *Command,
                                                SourceLocation ArgLocBegin,
                                                SourceLocation ArgLocEnd,
                                                StringRef Arg) {
  // Parser will not feed us more arguments than needed.
  assert(Command->getArgCount() == 0);

  if (!Command->isDirectionExplicit()) {
    // User didn't provide a direction argument.
    Command->setDirection(ParamCommandComment::In, /* Explicit = */ false);
  }
  typedef BlockCommandComment::Argument Argument;
  Argument *A = new (Allocator) Argument(SourceRange(ArgLocBegin,
                                                     ArgLocEnd),
                                         Arg);
  Command->setArgs(llvm::makeArrayRef(A, 1));

  if (!ThisDecl)
    return Command;

  const ParmVarDecl * const *ParamVars;
  unsigned NumParams;
  if (const FunctionDecl *FD = dyn_cast<FunctionDecl>(ThisDecl)) {
    ParamVars = FD->param_begin();
    NumParams = FD->getNumParams();
  } else if (const ObjCMethodDecl *MD = dyn_cast<ObjCMethodDecl>(ThisDecl)) {
    ParamVars = MD->param_begin();
    NumParams = MD->param_size();
  } else {
    // We already warned that this \\param is not attached to a function decl.
    return Command;
  }

  // Check that referenced parameter name is in the function decl.
  const unsigned ResolvedParamIndex = resolveParmVarReference(Arg, ParamVars,
                                                              NumParams);
  if (ResolvedParamIndex != ParamCommandComment::InvalidParamIndex) {
    Command->setParamIndex(ResolvedParamIndex);
    return Command;
  }

  SourceRange ArgRange(ArgLocBegin, ArgLocEnd);
  Diag(ArgLocBegin, diag::warn_doc_param_not_found)
    << Arg << ArgRange;

  unsigned CorrectedParamIndex = ParamCommandComment::InvalidParamIndex;
  if (NumParams == 1) {
    // If function has only one parameter then only that parameter
    // can be documented.
    CorrectedParamIndex = 0;
  } else {
    // Do typo correction.
    CorrectedParamIndex = correctTypoInParmVarReference(Arg, ParamVars,
                                                        NumParams);
  }
  if (CorrectedParamIndex != ParamCommandComment::InvalidParamIndex) {
    const ParmVarDecl *CorrectedPVD = ParamVars[CorrectedParamIndex];
    if (const IdentifierInfo *CorrectedII = CorrectedPVD->getIdentifier())
      Diag(ArgLocBegin, diag::note_doc_param_name_suggestion)
        << CorrectedII->getName()
        << FixItHint::CreateReplacement(ArgRange, CorrectedII->getName());
  }

  return Command;
}

ParamCommandComment *Sema::actOnParamCommandFinish(ParamCommandComment *Command,
                                                   ParagraphComment *Paragraph) {
  Command->setParagraph(Paragraph);
  checkBlockCommandEmptyParagraph(Command);
  return Command;
}

InlineCommandComment *Sema::actOnInlineCommand(SourceLocation CommandLocBegin,
                                               SourceLocation CommandLocEnd,
                                               StringRef CommandName) {
  ArrayRef<InlineCommandComment::Argument> Args;
  return new (Allocator) InlineCommandComment(CommandLocBegin,
                                              CommandLocEnd,
                                              CommandName,
                                              Args);
}

InlineCommandComment *Sema::actOnInlineCommand(SourceLocation CommandLocBegin,
                                               SourceLocation CommandLocEnd,
                                               StringRef CommandName,
                                               SourceLocation ArgLocBegin,
                                               SourceLocation ArgLocEnd,
                                               StringRef Arg) {
  typedef InlineCommandComment::Argument Argument;
  Argument *A = new (Allocator) Argument(SourceRange(ArgLocBegin,
                                                     ArgLocEnd),
                                         Arg);

  return new (Allocator) InlineCommandComment(CommandLocBegin,
                                              CommandLocEnd,
                                              CommandName,
                                              llvm::makeArrayRef(A, 1));
}

InlineContentComment *Sema::actOnUnknownCommand(SourceLocation LocBegin,
                                                SourceLocation LocEnd,
                                                StringRef Name) {
  ArrayRef<InlineCommandComment::Argument> Args;
  return new (Allocator) InlineCommandComment(LocBegin, LocEnd, Name, Args);
}

TextComment *Sema::actOnText(SourceLocation LocBegin,
                             SourceLocation LocEnd,
                             StringRef Text) {
  return new (Allocator) TextComment(LocBegin, LocEnd, Text);
}

VerbatimBlockComment *Sema::actOnVerbatimBlockStart(SourceLocation Loc,
                                                    StringRef Name) {
  return new (Allocator) VerbatimBlockComment(
                                  Loc,
                                  Loc.getLocWithOffset(1 + Name.size()),
                                  Name);
}

VerbatimBlockLineComment *Sema::actOnVerbatimBlockLine(SourceLocation Loc,
                                                       StringRef Text) {
  return new (Allocator) VerbatimBlockLineComment(Loc, Text);
}

VerbatimBlockComment *Sema::actOnVerbatimBlockFinish(
                            VerbatimBlockComment *Block,
                            SourceLocation CloseNameLocBegin,
                            StringRef CloseName,
                            ArrayRef<VerbatimBlockLineComment *> Lines) {
  Block->setCloseName(CloseName, CloseNameLocBegin);
  Block->setLines(Lines);
  return Block;
}

VerbatimLineComment *Sema::actOnVerbatimLine(SourceLocation LocBegin,
                                             StringRef Name,
                                             SourceLocation TextBegin,
                                             StringRef Text) {
  return new (Allocator) VerbatimLineComment(
                              LocBegin,
                              TextBegin.getLocWithOffset(Text.size()),
                              Name,
                              TextBegin,
                              Text);
}

HTMLOpenTagComment *Sema::actOnHTMLOpenTagStart(SourceLocation LocBegin,
                                                StringRef TagName) {
  HTMLOpenTagComment *HOT =
      new (Allocator) HTMLOpenTagComment(LocBegin, TagName);
  return HOT;
}

HTMLOpenTagComment *Sema::actOnHTMLOpenTagFinish(
                              HTMLOpenTagComment *Tag,
                              ArrayRef<HTMLOpenTagComment::Attribute> Attrs,
                              SourceLocation GreaterLoc,
                              bool IsSelfClosing) {
  Tag->setAttrs(Attrs);
  Tag->setGreaterLoc(GreaterLoc);
  if (IsSelfClosing)
    Tag->setSelfClosing();
  else if (!isHTMLCloseTagForbidden(Tag->getTagName()))
    HTMLOpenTags.push_back(Tag);
  return Tag;
}

HTMLCloseTagComment *Sema::actOnHTMLCloseTag(SourceLocation LocBegin,
                                             SourceLocation LocEnd,
                                             StringRef TagName) {
  HTMLCloseTagComment *HCT =
      new (Allocator) HTMLCloseTagComment(LocBegin, LocEnd, TagName);
  if (isHTMLCloseTagForbidden(TagName)) {
    Diag(HCT->getLocation(), diag::warn_doc_html_close_forbidden)
      << TagName << HCT->getSourceRange();
    return HCT;
  }

  bool FoundOpen = false;
  for (SmallVectorImpl<HTMLOpenTagComment *>::const_reverse_iterator
       I = HTMLOpenTags.rbegin(), E = HTMLOpenTags.rend();
       I != E; ++I) {
    if ((*I)->getTagName() == TagName) {
      FoundOpen = true;
      break;
    }
  }
  if (!FoundOpen) {
    Diag(HCT->getLocation(), diag::warn_doc_html_close_unbalanced)
      << HCT->getSourceRange();
    return HCT;
  }

  while (!HTMLOpenTags.empty()) {
    const HTMLOpenTagComment *HOT = HTMLOpenTags.back();
    HTMLOpenTags.pop_back();
    StringRef LastNotClosedTagName = HOT->getTagName();
    if (LastNotClosedTagName == TagName)
      break;

    if (isHTMLCloseTagOptional(LastNotClosedTagName))
      continue;

    bool OpenLineInvalid;
    const unsigned OpenLine = SourceMgr.getPresumedLineNumber(
                                                HOT->getLocation(),
                                                &OpenLineInvalid);
    bool CloseLineInvalid;
    const unsigned CloseLine = SourceMgr.getPresumedLineNumber(
                                                HCT->getLocation(),
                                                &CloseLineInvalid);

    if (OpenLineInvalid || CloseLineInvalid || OpenLine == CloseLine)
      Diag(HOT->getLocation(), diag::warn_doc_html_open_close_mismatch)
        << HOT->getTagName() << HCT->getTagName()
        << HOT->getSourceRange() << HCT->getSourceRange();
    else {
      Diag(HOT->getLocation(), diag::warn_doc_html_open_close_mismatch)
        << HOT->getTagName() << HCT->getTagName()
        << HOT->getSourceRange();
      Diag(HCT->getLocation(), diag::note_doc_html_closing_tag)
        << HCT->getSourceRange();
    }
  }

  return HCT;
}

FullComment *Sema::actOnFullComment(
                              ArrayRef<BlockContentComment *> Blocks) {
  return new (Allocator) FullComment(Blocks);
}

void Sema::checkBlockCommandEmptyParagraph(BlockCommandComment *Command) {
  ParagraphComment *Paragraph = Command->getParagraph();
  if (Paragraph->isWhitespace()) {
    SourceLocation DiagLoc;
    if (Command->getArgCount() > 0)
      DiagLoc = Command->getArgRange(Command->getArgCount() - 1).getEnd();
    if (!DiagLoc.isValid())
      DiagLoc = Command->getCommandNameRange().getEnd();
    Diag(DiagLoc, diag::warn_doc_block_command_empty_paragraph)
      << Command->getCommandName()
      << Command->getSourceRange();
  }
}

unsigned Sema::resolveParmVarReference(StringRef Name,
                                       const ParmVarDecl * const *ParamVars,
                                       unsigned NumParams) {
  for (unsigned i = 0; i != NumParams; ++i) {
    const IdentifierInfo *II = ParamVars[i]->getIdentifier();
    if (II && II->getName() == Name)
      return i;
  }
  return ParamCommandComment::InvalidParamIndex;
}

unsigned Sema::correctTypoInParmVarReference(
                                    StringRef Typo,
                                    const ParmVarDecl * const *ParamVars,
                                    unsigned NumParams) {
  const unsigned MaxEditDistance = (Typo.size() + 2) / 3;
  unsigned BestPVDIndex = 0;
  unsigned BestEditDistance = MaxEditDistance + 1;
  for (unsigned i = 0; i != NumParams; ++i) {
    const IdentifierInfo *II = ParamVars[i]->getIdentifier();
    if (II) {
      StringRef Name = II->getName();
      unsigned MinPossibleEditDistance =
        abs((int)Name.size() - (int)Typo.size());
      if (MinPossibleEditDistance > 0 &&
          Typo.size() / MinPossibleEditDistance < 3)
        continue;

      unsigned EditDistance = Typo.edit_distance(Name, true, MaxEditDistance);
      if (EditDistance < BestEditDistance) {
        BestEditDistance = EditDistance;
        BestPVDIndex = i;
      }
    }
  }

  if (BestEditDistance <= MaxEditDistance)
    return BestPVDIndex;
  else
    return ParamCommandComment::InvalidParamIndex;;
}

// TODO: tablegen
bool Sema::isBlockCommand(StringRef Name) {
  return llvm::StringSwitch<bool>(Name)
      .Case("brief", true)
      .Case("result", true)
      .Case("return", true)
      .Case("returns", true)
      .Case("author", true)
      .Case("authors", true)
      .Case("pre", true)
      .Case("post", true)
      .Default(false) || isParamCommand(Name);
}

bool Sema::isParamCommand(StringRef Name) {
  return llvm::StringSwitch<bool>(Name)
      .Case("param", true)
      .Case("arg", true)
      .Default(false);
}

unsigned Sema::getBlockCommandNumArgs(StringRef Name) {
  return llvm::StringSwitch<unsigned>(Name)
      .Case("brief", 0)
      .Case("pre", 0)
      .Case("post", 0)
      .Case("author", 0)
      .Case("authors", 0)
      .Default(0);
}

bool Sema::isInlineCommand(StringRef Name) {
  return llvm::StringSwitch<bool>(Name)
      .Case("c", true)
      .Case("em", true)
      .Default(false);
}

bool Sema::isHTMLCloseTagOptional(StringRef Name) {
  return llvm::StringSwitch<bool>(Name)
      .Case("p", true)
      .Case("li", true)
      .Case("dt", true)
      .Case("dd", true)
      .Case("tr", true)
      .Case("th", true)
      .Case("td", true)
      .Case("thead", true)
      .Case("tfoot", true)
      .Case("tbody", true)
      .Case("colgroup", true)
      .Default(false);
}

bool Sema::isHTMLCloseTagForbidden(StringRef Name) {
  return llvm::StringSwitch<bool>(Name)
      .Case("br", true)
      .Case("hr", true)
      .Case("img", true)
      .Case("col", true)
      .Default(false);
}

} // end namespace comments
} // end namespace clang

