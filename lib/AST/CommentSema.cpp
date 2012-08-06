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
#include "clang/AST/DeclTemplate.h"
#include "clang/Basic/SourceManager.h"
#include "llvm/ADT/StringSwitch.h"

namespace clang {
namespace comments {

Sema::Sema(llvm::BumpPtrAllocator &Allocator, const SourceManager &SourceMgr,
           DiagnosticsEngine &Diags) :
    Allocator(Allocator), SourceMgr(SourceMgr), Diags(Diags),
    ThisDeclInfo(NULL), BriefCommand(NULL), ReturnsCommand(NULL) {
}

void Sema::setDecl(const Decl *D) {
  if (!D)
    return;

  ThisDeclInfo = new (Allocator) DeclInfo;
  ThisDeclInfo->ThisDecl = D;
  ThisDeclInfo->IsFilled = false;
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

void Sema::actOnBlockCommandArgs(BlockCommandComment *Command,
                                 ArrayRef<BlockCommandComment::Argument> Args) {
  Command->setArgs(Args);
}

void Sema::actOnBlockCommandFinish(BlockCommandComment *Command,
                                   ParagraphComment *Paragraph) {
  Command->setParagraph(Paragraph);
  checkBlockCommandEmptyParagraph(Command);
  checkBlockCommandDuplicate(Command);
  checkReturnsCommand(Command);
}

ParamCommandComment *Sema::actOnParamCommandStart(SourceLocation LocBegin,
                                                  SourceLocation LocEnd,
                                                  StringRef Name) {
  ParamCommandComment *Command =
      new (Allocator) ParamCommandComment(LocBegin, LocEnd, Name);

  if (!isFunctionDecl())
    Diag(Command->getLocation(),
         diag::warn_doc_param_not_attached_to_a_function_decl)
      << Command->getCommandNameRange();

  return Command;
}

void Sema::actOnParamCommandDirectionArg(ParamCommandComment *Command,
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
}

void Sema::actOnParamCommandParamNameArg(ParamCommandComment *Command,
                                         SourceLocation ArgLocBegin,
                                         SourceLocation ArgLocEnd,
                                         StringRef Arg) {
  // Parser will not feed us more arguments than needed.
  assert(Command->getNumArgs() == 0);

  if (!Command->isDirectionExplicit()) {
    // User didn't provide a direction argument.
    Command->setDirection(ParamCommandComment::In, /* Explicit = */ false);
  }
  typedef BlockCommandComment::Argument Argument;
  Argument *A = new (Allocator) Argument(SourceRange(ArgLocBegin,
                                                     ArgLocEnd),
                                         Arg);
  Command->setArgs(llvm::makeArrayRef(A, 1));

  if (!isFunctionDecl()) {
    // We already warned that this \\param is not attached to a function decl.
    return;
  }

  ArrayRef<const ParmVarDecl *> ParamVars = getParamVars();

  // Check that referenced parameter name is in the function decl.
  const unsigned ResolvedParamIndex = resolveParmVarReference(Arg, ParamVars);
  if (ResolvedParamIndex != ParamCommandComment::InvalidParamIndex) {
    Command->setParamIndex(ResolvedParamIndex);
    if (ParamVarDocs[ResolvedParamIndex]) {
      SourceRange ArgRange(ArgLocBegin, ArgLocEnd);
      Diag(ArgLocBegin, diag::warn_doc_param_duplicate)
        << Arg << ArgRange;
      ParamCommandComment *PrevCommand = ParamVarDocs[ResolvedParamIndex];
      Diag(PrevCommand->getLocation(), diag::note_doc_param_previous)
        << PrevCommand->getParamNameRange();
    }
    ParamVarDocs[ResolvedParamIndex] = Command;
    return;
  }

  SourceRange ArgRange(ArgLocBegin, ArgLocEnd);
  Diag(ArgLocBegin, diag::warn_doc_param_not_found)
    << Arg << ArgRange;

  // No parameters -- can't suggest a correction.
  if (ParamVars.size() == 0)
    return;

  unsigned CorrectedParamIndex = ParamCommandComment::InvalidParamIndex;
  if (ParamVars.size() == 1) {
    // If function has only one parameter then only that parameter
    // can be documented.
    CorrectedParamIndex = 0;
  } else {
    // Do typo correction.
    CorrectedParamIndex = correctTypoInParmVarReference(Arg, ParamVars);
  }
  if (CorrectedParamIndex != ParamCommandComment::InvalidParamIndex) {
    const ParmVarDecl *CorrectedPVD = ParamVars[CorrectedParamIndex];
    if (const IdentifierInfo *CorrectedII = CorrectedPVD->getIdentifier())
      Diag(ArgLocBegin, diag::note_doc_param_name_suggestion)
        << CorrectedII->getName()
        << FixItHint::CreateReplacement(ArgRange, CorrectedII->getName());
  }

  return;
}

void Sema::actOnParamCommandFinish(ParamCommandComment *Command,
                                   ParagraphComment *Paragraph) {
  Command->setParagraph(Paragraph);
  checkBlockCommandEmptyParagraph(Command);
}

TParamCommandComment *Sema::actOnTParamCommandStart(SourceLocation LocBegin,
                                                    SourceLocation LocEnd,
                                                    StringRef Name) {
  TParamCommandComment *Command =
      new (Allocator) TParamCommandComment(LocBegin, LocEnd, Name);

  if (!isTemplateOrSpecialization())
    Diag(Command->getLocation(),
         diag::warn_doc_tparam_not_attached_to_a_template_decl)
      << Command->getCommandNameRange();

  return Command;
}

void Sema::actOnTParamCommandParamNameArg(TParamCommandComment *Command,
                                          SourceLocation ArgLocBegin,
                                          SourceLocation ArgLocEnd,
                                          StringRef Arg) {
  // Parser will not feed us more arguments than needed.
  assert(Command->getNumArgs() == 0);

  typedef BlockCommandComment::Argument Argument;
  Argument *A = new (Allocator) Argument(SourceRange(ArgLocBegin,
                                                     ArgLocEnd),
                                         Arg);
  Command->setArgs(llvm::makeArrayRef(A, 1));

  if (!isTemplateOrSpecialization()) {
    // We already warned that this \\tparam is not attached to a template decl.
    return;
  }

  const TemplateParameterList *TemplateParameters =
      ThisDeclInfo->TemplateParameters;
  SmallVector<unsigned, 2> Position;
  if (resolveTParamReference(Arg, TemplateParameters, &Position)) {
    Command->setPosition(copyArray(llvm::makeArrayRef(Position)));
    llvm::StringMap<TParamCommandComment *>::iterator PrevCommandIt =
        TemplateParameterDocs.find(Arg);
    if (PrevCommandIt != TemplateParameterDocs.end()) {
      SourceRange ArgRange(ArgLocBegin, ArgLocEnd);
      Diag(ArgLocBegin, diag::warn_doc_tparam_duplicate)
        << Arg << ArgRange;
      TParamCommandComment *PrevCommand = PrevCommandIt->second;
      Diag(PrevCommand->getLocation(), diag::note_doc_tparam_previous)
        << PrevCommand->getParamNameRange();
    }
    TemplateParameterDocs[Arg] = Command;
    return;
  }

  SourceRange ArgRange(ArgLocBegin, ArgLocEnd);
  Diag(ArgLocBegin, diag::warn_doc_tparam_not_found)
    << Arg << ArgRange;

  if (!TemplateParameters || TemplateParameters->size() == 0)
    return;

  StringRef CorrectedName;
  if (TemplateParameters->size() == 1) {
    const NamedDecl *Param = TemplateParameters->getParam(0);
    const IdentifierInfo *II = Param->getIdentifier();
    if (II)
      CorrectedName = II->getName();
  } else {
    CorrectedName = correctTypoInTParamReference(Arg, TemplateParameters);
  }

  if (!CorrectedName.empty()) {
    Diag(ArgLocBegin, diag::note_doc_tparam_name_suggestion)
      << CorrectedName
      << FixItHint::CreateReplacement(ArgRange, CorrectedName);
  }

  return;
}

void Sema::actOnTParamCommandFinish(TParamCommandComment *Command,
                                    ParagraphComment *Paragraph) {
  Command->setParagraph(Paragraph);
  checkBlockCommandEmptyParagraph(Command);
}

InlineCommandComment *Sema::actOnInlineCommand(SourceLocation CommandLocBegin,
                                               SourceLocation CommandLocEnd,
                                               StringRef CommandName) {
  ArrayRef<InlineCommandComment::Argument> Args;
  return new (Allocator) InlineCommandComment(
                                  CommandLocBegin,
                                  CommandLocEnd,
                                  CommandName,
                                  getInlineCommandRenderKind(CommandName),
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

  return new (Allocator) InlineCommandComment(
                                  CommandLocBegin,
                                  CommandLocEnd,
                                  CommandName,
                                  getInlineCommandRenderKind(CommandName),
                                  llvm::makeArrayRef(A, 1));
}

InlineContentComment *Sema::actOnUnknownCommand(SourceLocation LocBegin,
                                                SourceLocation LocEnd,
                                                StringRef Name) {
  ArrayRef<InlineCommandComment::Argument> Args;
  return new (Allocator) InlineCommandComment(
                                  LocBegin, LocEnd, Name,
                                  InlineCommandComment::RenderNormal,
                                  Args);
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

void Sema::actOnVerbatimBlockFinish(
                            VerbatimBlockComment *Block,
                            SourceLocation CloseNameLocBegin,
                            StringRef CloseName,
                            ArrayRef<VerbatimBlockLineComment *> Lines) {
  Block->setCloseName(CloseName, CloseNameLocBegin);
  Block->setLines(Lines);
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

HTMLStartTagComment *Sema::actOnHTMLStartTagStart(SourceLocation LocBegin,
                                                  StringRef TagName) {
  return new (Allocator) HTMLStartTagComment(LocBegin, TagName);
}

void Sema::actOnHTMLStartTagFinish(
                              HTMLStartTagComment *Tag,
                              ArrayRef<HTMLStartTagComment::Attribute> Attrs,
                              SourceLocation GreaterLoc,
                              bool IsSelfClosing) {
  Tag->setAttrs(Attrs);
  Tag->setGreaterLoc(GreaterLoc);
  if (IsSelfClosing)
    Tag->setSelfClosing();
  else if (!isHTMLEndTagForbidden(Tag->getTagName()))
    HTMLOpenTags.push_back(Tag);
}

HTMLEndTagComment *Sema::actOnHTMLEndTag(SourceLocation LocBegin,
                                         SourceLocation LocEnd,
                                         StringRef TagName) {
  HTMLEndTagComment *HET =
      new (Allocator) HTMLEndTagComment(LocBegin, LocEnd, TagName);
  if (isHTMLEndTagForbidden(TagName)) {
    Diag(HET->getLocation(), diag::warn_doc_html_end_forbidden)
      << TagName << HET->getSourceRange();
    return HET;
  }

  bool FoundOpen = false;
  for (SmallVectorImpl<HTMLStartTagComment *>::const_reverse_iterator
       I = HTMLOpenTags.rbegin(), E = HTMLOpenTags.rend();
       I != E; ++I) {
    if ((*I)->getTagName() == TagName) {
      FoundOpen = true;
      break;
    }
  }
  if (!FoundOpen) {
    Diag(HET->getLocation(), diag::warn_doc_html_end_unbalanced)
      << HET->getSourceRange();
    return HET;
  }

  while (!HTMLOpenTags.empty()) {
    const HTMLStartTagComment *HST = HTMLOpenTags.back();
    HTMLOpenTags.pop_back();
    StringRef LastNotClosedTagName = HST->getTagName();
    if (LastNotClosedTagName == TagName)
      break;

    if (isHTMLEndTagOptional(LastNotClosedTagName))
      continue;

    bool OpenLineInvalid;
    const unsigned OpenLine = SourceMgr.getPresumedLineNumber(
                                                HST->getLocation(),
                                                &OpenLineInvalid);
    bool CloseLineInvalid;
    const unsigned CloseLine = SourceMgr.getPresumedLineNumber(
                                                HET->getLocation(),
                                                &CloseLineInvalid);

    if (OpenLineInvalid || CloseLineInvalid || OpenLine == CloseLine)
      Diag(HST->getLocation(), diag::warn_doc_html_start_end_mismatch)
        << HST->getTagName() << HET->getTagName()
        << HST->getSourceRange() << HET->getSourceRange();
    else {
      Diag(HST->getLocation(), diag::warn_doc_html_start_end_mismatch)
        << HST->getTagName() << HET->getTagName()
        << HST->getSourceRange();
      Diag(HET->getLocation(), diag::note_doc_html_end_tag)
        << HET->getSourceRange();
    }
  }

  return HET;
}

FullComment *Sema::actOnFullComment(
                              ArrayRef<BlockContentComment *> Blocks) {
  return new (Allocator) FullComment(Blocks, ThisDeclInfo);
}

void Sema::checkBlockCommandEmptyParagraph(BlockCommandComment *Command) {
  ParagraphComment *Paragraph = Command->getParagraph();
  if (Paragraph->isWhitespace()) {
    SourceLocation DiagLoc;
    if (Command->getNumArgs() > 0)
      DiagLoc = Command->getArgRange(Command->getNumArgs() - 1).getEnd();
    if (!DiagLoc.isValid())
      DiagLoc = Command->getCommandNameRange().getEnd();
    Diag(DiagLoc, diag::warn_doc_block_command_empty_paragraph)
      << Command->getCommandName()
      << Command->getSourceRange();
  }
}

void Sema::checkReturnsCommand(const BlockCommandComment *Command) {
  if (!isReturnsCommand(Command->getCommandName()))
    return;
  if (isFunctionDecl()) {
    if (ThisDeclInfo->ResultType->isVoidType()) {
      unsigned DiagKind;
      switch (ThisDeclInfo->ThisDecl->getKind()) {
      default:
        if (ThisDeclInfo->IsObjCMethod)
          DiagKind = 3;
        else
          DiagKind = 0;
        break;
      case Decl::CXXConstructor:
        DiagKind = 1;
        break;
      case Decl::CXXDestructor:
        DiagKind = 2;
        break;
      }
      Diag(Command->getLocation(),
           diag::warn_doc_returns_attached_to_a_void_function)
        << Command->getCommandName()
        << DiagKind
        << Command->getSourceRange();
    }
    return;
  }
  Diag(Command->getLocation(),
       diag::warn_doc_returns_not_attached_to_a_function_decl)
    << Command->getCommandName()
    << Command->getSourceRange();
}

void Sema::checkBlockCommandDuplicate(const BlockCommandComment *Command) {
  StringRef Name = Command->getCommandName();
  const BlockCommandComment *PrevCommand = NULL;
  if (isBriefCommand(Name)) {
    if (!BriefCommand) {
      BriefCommand = Command;
      return;
    }
    PrevCommand = BriefCommand;
  } else if (isReturnsCommand(Name)) {
    if (!ReturnsCommand) {
      ReturnsCommand = Command;
      return;
    }
    PrevCommand = ReturnsCommand;
  } else {
    // We don't want to check this command for duplicates.
    return;
  }
  Diag(Command->getLocation(), diag::warn_doc_block_command_duplicate)
      << Name
      << Command->getSourceRange();
  if (Name == PrevCommand->getCommandName())
    Diag(PrevCommand->getLocation(), diag::note_doc_block_command_previous)
        << PrevCommand->getCommandName()
        << Command->getSourceRange();
  else
    Diag(PrevCommand->getLocation(),
         diag::note_doc_block_command_previous_alias)
        << PrevCommand->getCommandName()
        << Name;
}

bool Sema::isFunctionDecl() {
  if (!ThisDeclInfo)
    return false;
  if (!ThisDeclInfo->IsFilled)
    inspectThisDecl();
  return ThisDeclInfo->getKind() == DeclInfo::FunctionKind;
}

bool Sema::isTemplateOrSpecialization() {
  if (!ThisDeclInfo)
    return false;
  if (!ThisDeclInfo->IsFilled)
    inspectThisDecl();
  return ThisDeclInfo->getTemplateKind() != DeclInfo::NotTemplate;
}

ArrayRef<const ParmVarDecl *> Sema::getParamVars() {
  if (!ThisDeclInfo->IsFilled)
    inspectThisDecl();
  return ThisDeclInfo->ParamVars;
}

void Sema::inspectThisDecl() {
  ThisDeclInfo->fill();
  ParamVarDocs.resize(ThisDeclInfo->ParamVars.size(), NULL);
}

unsigned Sema::resolveParmVarReference(StringRef Name,
                                       ArrayRef<const ParmVarDecl *> ParamVars) {
  for (unsigned i = 0, e = ParamVars.size(); i != e; ++i) {
    const IdentifierInfo *II = ParamVars[i]->getIdentifier();
    if (II && II->getName() == Name)
      return i;
  }
  return ParamCommandComment::InvalidParamIndex;
}

namespace {
class SimpleTypoCorrector {
  StringRef Typo;
  const unsigned MaxEditDistance;

  const NamedDecl *BestDecl;
  unsigned BestEditDistance;
  unsigned BestIndex;
  unsigned NextIndex;

public:
  SimpleTypoCorrector(StringRef Typo) :
      Typo(Typo), MaxEditDistance((Typo.size() + 2) / 3),
      BestDecl(NULL), BestEditDistance(MaxEditDistance + 1),
      BestIndex(0), NextIndex(0)
  { }

  void addDecl(const NamedDecl *ND);

  const NamedDecl *getBestDecl() const {
    if (BestEditDistance > MaxEditDistance)
      return NULL;

    return BestDecl;
  }

  unsigned getBestDeclIndex() const {
    assert(getBestDecl());
    return BestIndex;
  }
};

void SimpleTypoCorrector::addDecl(const NamedDecl *ND) {
  unsigned CurrIndex = NextIndex++;

  const IdentifierInfo *II = ND->getIdentifier();
  if (!II)
    return;

  StringRef Name = II->getName();
  unsigned MinPossibleEditDistance = abs((int)Name.size() - (int)Typo.size());
  if (MinPossibleEditDistance > 0 &&
      Typo.size() / MinPossibleEditDistance < 3)
    return;

  unsigned EditDistance = Typo.edit_distance(Name, true, MaxEditDistance);
  if (EditDistance < BestEditDistance) {
    BestEditDistance = EditDistance;
    BestDecl = ND;
    BestIndex = CurrIndex;
  }
}
} // unnamed namespace

unsigned Sema::correctTypoInParmVarReference(
                                    StringRef Typo,
                                    ArrayRef<const ParmVarDecl *> ParamVars) {
  SimpleTypoCorrector Corrector(Typo);
  for (unsigned i = 0, e = ParamVars.size(); i != e; ++i)
    Corrector.addDecl(ParamVars[i]);
  if (Corrector.getBestDecl())
    return Corrector.getBestDeclIndex();
  else
    return ParamCommandComment::InvalidParamIndex;;
}

namespace {
bool ResolveTParamReferenceHelper(
                            StringRef Name,
                            const TemplateParameterList *TemplateParameters,
                            SmallVectorImpl<unsigned> *Position) {
  for (unsigned i = 0, e = TemplateParameters->size(); i != e; ++i) {
    const NamedDecl *Param = TemplateParameters->getParam(i);
    const IdentifierInfo *II = Param->getIdentifier();
    if (II && II->getName() == Name) {
      Position->push_back(i);
      return true;
    }

    if (const TemplateTemplateParmDecl *TTP =
            dyn_cast<TemplateTemplateParmDecl>(Param)) {
      Position->push_back(i);
      if (ResolveTParamReferenceHelper(Name, TTP->getTemplateParameters(),
                                       Position))
        return true;
      Position->pop_back();
    }
  }
  return false;
}
} // unnamed namespace

bool Sema::resolveTParamReference(
                            StringRef Name,
                            const TemplateParameterList *TemplateParameters,
                            SmallVectorImpl<unsigned> *Position) {
  Position->clear();
  if (!TemplateParameters)
    return false;

  return ResolveTParamReferenceHelper(Name, TemplateParameters, Position);
}

namespace {
void CorrectTypoInTParamReferenceHelper(
                            const TemplateParameterList *TemplateParameters,
                            SimpleTypoCorrector &Corrector) {
  for (unsigned i = 0, e = TemplateParameters->size(); i != e; ++i) {
    const NamedDecl *Param = TemplateParameters->getParam(i);
    Corrector.addDecl(Param);

    if (const TemplateTemplateParmDecl *TTP =
            dyn_cast<TemplateTemplateParmDecl>(Param))
      CorrectTypoInTParamReferenceHelper(TTP->getTemplateParameters(),
                                         Corrector);
  }
}
} // unnamed namespace

StringRef Sema::correctTypoInTParamReference(
                            StringRef Typo,
                            const TemplateParameterList *TemplateParameters) {
  SimpleTypoCorrector Corrector(Typo);
  CorrectTypoInTParamReferenceHelper(TemplateParameters, Corrector);
  if (const NamedDecl *ND = Corrector.getBestDecl()) {
    const IdentifierInfo *II = ND->getIdentifier();
    assert(II && "SimpleTypoCorrector should not return this decl");
    return II->getName();
  }
  return StringRef();
}

// TODO: tablegen
bool Sema::isBlockCommand(StringRef Name) {
  return isBriefCommand(Name) || isReturnsCommand(Name) ||
      isParamCommand(Name) || isTParamCommand(Name) ||
      llvm::StringSwitch<bool>(Name)
      .Case("author", true)
      .Case("authors", true)
      .Case("pre", true)
      .Case("post", true)
      .Default(false);
}

bool Sema::isParamCommand(StringRef Name) {
  return llvm::StringSwitch<bool>(Name)
      .Case("param", true)
      .Case("arg", true)
      .Default(false);
}

bool Sema::isTParamCommand(StringRef Name) {
  return Name == "tparam";
}

bool Sema::isBriefCommand(StringRef Name) {
  return Name == "brief" || Name == "short";
}

bool Sema::isReturnsCommand(StringRef Name) {
  return Name == "returns" || Name == "return" || Name == "result";
}

unsigned Sema::getBlockCommandNumArgs(StringRef Name) {
  return llvm::StringSwitch<unsigned>(Name)
      .Cases("brief", "short", 0)
      .Case("pre", 0)
      .Case("post", 0)
      .Case("author", 0)
      .Case("authors", 0)
      .Default(0);
}

bool Sema::isInlineCommand(StringRef Name) const {
  return llvm::StringSwitch<bool>(Name)
      .Case("b", true)
      .Cases("c", "p", true)
      .Cases("a", "e", "em", true)
      .Default(false);
}

InlineCommandComment::RenderKind
Sema::getInlineCommandRenderKind(StringRef Name) const {
  assert(isInlineCommand(Name));

  return llvm::StringSwitch<InlineCommandComment::RenderKind>(Name)
      .Case("b", InlineCommandComment::RenderBold)
      .Cases("c", "p", InlineCommandComment::RenderMonospaced)
      .Cases("a", "e", "em", InlineCommandComment::RenderEmphasized)
      .Default(InlineCommandComment::RenderNormal);
}

bool Sema::isHTMLEndTagOptional(StringRef Name) {
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

bool Sema::isHTMLEndTagForbidden(StringRef Name) {
  return llvm::StringSwitch<bool>(Name)
      .Case("br", true)
      .Case("hr", true)
      .Case("img", true)
      .Case("col", true)
      .Default(false);
}

} // end namespace comments
} // end namespace clang

