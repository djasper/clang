//===--- Format.cpp - Format C++ code -------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief This file implements functions declared in Format.h. This will be
/// split into separate files as we go.
///
/// This is EXPERIMENTAL code under heavy development. It is not in a state yet,
/// where it can be used to format real code.
///
//===----------------------------------------------------------------------===//

#include "clang/Format/Format.h"

#include "clang/Basic/SourceManager.h"
#include "clang/Lex/Lexer.h"

#include "UnwrappedLineParser.h"

namespace clang {
namespace format {

using llvm::MutableArrayRef;

FormatStyle getLLVMStyle() {
  FormatStyle LLVMStyle;
  LLVMStyle.ColumnLimit = 80;
  LLVMStyle.MaxEmptyLinesToKeep = 1;
  LLVMStyle.PointerAndReferenceBindToType = false;
  LLVMStyle.AccessModifierOffset = -2;
  return LLVMStyle;
}

FormatStyle getGoogleStyle() {
  FormatStyle GoogleStyle;
  GoogleStyle.ColumnLimit = 80;
  GoogleStyle.MaxEmptyLinesToKeep = 1;
  GoogleStyle.PointerAndReferenceBindToType = true;
  GoogleStyle.AccessModifierOffset = -1;
  return GoogleStyle;
}

struct OptimizationParameters {
  unsigned PenaltyExtraLine;
  unsigned PenaltyIndentLevel;
};

class UnwrappedLineFormatter {
public:
  UnwrappedLineFormatter(const FormatStyle &Style, SourceManager &SourceMgr,
                         const UnwrappedLine &Line,
                         tooling::Replacements &Replaces)
      : Style(Style),
        SourceMgr(SourceMgr),
        Line(Line),
        Replaces(Replaces) {
    Parameters.PenaltyExtraLine = 100;
    Parameters.PenaltyIndentLevel = 1;
  }

  void format() {
    analyzeTokens();
    addNewline(Line.Tokens[0], Line.Level);
    count = 0;
    IndentState State;
    State.Column = Line.Level * 2 + Line.Tokens[0].Tok.getLength();
    State.CtorInitializerOnNewLine = false;
    State.InCtorInitializer = false;
    State.ConsumedTokens = 1;

    //State.UsedIndent.push_back(Line.Level * 2);
    State.Indent.push_back(Line.Level * 2 + 4);

    // Start iterating at 1 as we have correctly formatted of Token #0 above.
    for (unsigned i = 1, n = Line.Tokens.size(); i != n; ++i) {
      unsigned NoBreak = calcPenalty(State, false, UINT_MAX);
      unsigned Break = calcPenalty(State, true, NoBreak);
      //llvm::errs() << NoBreak << " " << Break << "\n";
      addToken(Break < NoBreak, false, State);
    }
    //llvm::errs() << count << "\n";
  }

private:
  /// \brief The current state when indenting a unwrapped line.
  ///
  /// As the indenting tries different combinations this is copied by value.
  struct IndentState {
    /// \brief The number of used columns in the current line.
    unsigned Column;

    unsigned ConsumedTokens;

    /// \brief The position to which a specific parenthesis level needs to be
    /// indented.
    std::vector<unsigned> Indent;

    bool CtorInitializerOnNewLine;
    bool InCtorInitializer;

    bool operator<(const IndentState &Other) const {
      if (Other.ConsumedTokens != ConsumedTokens)
        return Other.ConsumedTokens > ConsumedTokens;
      if (Other.Column != Column)
        return Other.Column > Column;
      if (Other.Indent.size() != Indent.size())
        return Other.Indent.size() > Indent.size();
      for (int i = 0, e = Indent.size(); i != e; ++i) {
        if (Other.Indent[i] != Indent[i])
          return Other.Indent[i] > Indent[i];
      }
      return false;
    }
  };

  /// Append the next token to \p State.
  void addToken(bool Newline, bool DryRun, IndentState &State) {
    unsigned Index = State.ConsumedTokens;
    unsigned ParenLevel = Annotations[Index].ParenLevel;

    if (Line.Tokens[Index].Tok.is(tok::l_paren) ||
        Line.Tokens[Index].Tok.is(tok::l_square) ||
        (Line.Tokens[Index].Tok.is(tok::less) && !Annotations[Index].IsOperator))
      State.Indent.push_back(4 + Line.Level * 2);

    if (Newline) {
      if (!DryRun)
        replaceWhitespace(Line.Tokens[Index], 1, State.Indent[ParenLevel]);
      State.Column = State.Indent[ParenLevel] +
          Line.Tokens[Index].Tok.getLength();
      if (Line.Tokens[Index].Tok.is(tok::colon) &&
          !Annotations[Index].IsTernaryExprColon) {
        State.Indent[ParenLevel] += 2;
        State.CtorInitializerOnNewLine = true;
        State.InCtorInitializer = true;
      }
    } else {
      unsigned Spaces = Annotations[Index].SpaceRequiredBefore ? 1 : 0;
      if (Line.Tokens[Index].Tok.is(tok::comment))
        Spaces = 2;
      if (!DryRun)
        replaceWhitespace(Line.Tokens[Index], 0, Spaces);
      if (Line.Tokens[Index - 1].Tok.is(tok::l_paren))
        State.Indent[ParenLevel] = State.Column;
      if (Line.Tokens[Index - 1].Tok.is(tok::less) &&
          !Annotations[Index - 1].IsOperator)
        State.Indent[ParenLevel] = State.Column;
      if (Line.Tokens[Index].Tok.is(tok::colon)) {
        State.Indent[ParenLevel] = State.Column + 3;
        State.InCtorInitializer = true;
      }
      State.Column += Line.Tokens[Index].Tok.getLength() + Spaces;
    }

    if (Line.Tokens[Index].Tok.is(tok::r_paren) ||
        Line.Tokens[Index].Tok.is(tok::r_square))
      State.Indent.pop_back();

    ++State.ConsumedTokens;
  }

  bool isTemplateOpener(unsigned Index) {
    unsigned ParenCount = 0;
    unsigned TemplateCount = 0;

    for (unsigned i = Index, e = Line.Tokens.size(); i != e; ++i) {
      switch(Line.Tokens[i].Tok.getKind()) {
      case tok::ampamp:
      case tok::pipepipe:
        return false;
        break;
      case tok::l_paren:
        ++ParenCount;
        break;
      case tok::r_paren:
        if (ParenCount == 0)
          return false;
        --ParenCount;
        break;
      case tok::less:
        ++TemplateCount;
        break;
      case tok::greater:
        --TemplateCount;
        if (TemplateCount == 0)
          return ParenCount == 0;
        break;
      case tok::greatergreater:
        --TemplateCount;
        if (TemplateCount == 0)
          return ParenCount == 0;
        --TemplateCount;
        if (TemplateCount == 0)
          return ParenCount == 0;
        break;
      default:
        break;
      }
    }
    return false;
  }

  struct TokenAnnotation {
    bool SpaceRequiredBefore;
    bool CanBreakBefore;
    bool IsTernaryExprColon;
    bool IsOperator;

    /// \brief The current parenthesis level, i.e. the number of opening minus
    /// the number of closing parenthesis le; of the current position.
    unsigned ParenLevel;
  };

  void analyzeTokens() {
    bool IsTernaryExpr = false;
    SmallVector<tok::TokenKind, 32> Parens;
    StringRef Text(SourceMgr.getCharacterData(Line.Tokens[0].Tok.getLocation()),
                   Line.Tokens[0].Tok.getLength());
    for (int i = 0, e = Line.Tokens.size(); i != e; ++i) {
      if (Line.Tokens[i].Tok.is(tok::question))
        IsTernaryExpr = true;
      TokenAnnotation Annotation;
      Annotation.IsOperator = false;
      Annotation.ParenLevel = Parens.size();

      const FormatToken &Token = Line.Tokens[i];
      if (Token.Tok.is(tok::l_paren) || Token.Tok.is(tok::l_square)) {
        Parens.push_back(Token.Tok.getKind());
      } else if (Token.Tok.is(tok::r_paren) || Token.Tok.is(tok::r_square)) {
        Parens.pop_back();
      } else if (Token.Tok.is(tok::less)) {
        Annotation.IsOperator = !isTemplateOpener(i);
        if (!Annotation.IsOperator)
          Parens.push_back(tok::less);
      } else if (Token.Tok.is(tok::greater)) {
        Annotation.IsOperator = Parens.back() != tok::less;
        if (!Annotation.IsOperator)
          Parens.pop_back();
      }

      if (i != 0)
        Annotation.CanBreakBefore =
            canBreakBetween(Line.Tokens[i - 1], Line.Tokens[i]);

      if (Line.Tokens[i].Tok.is(tok::colon)) {
        if (Text == "case") {
          Annotation.SpaceRequiredBefore = false;
        } else if (i == e - 1) {
          Annotation.SpaceRequiredBefore = false;
        } else {
          Annotation.IsTernaryExprColon = IsTernaryExpr;
          Annotation.SpaceRequiredBefore = true;
        }
      } else if (i == 0) {
        Annotation.SpaceRequiredBefore = false;
      } else if (i != 0) {
        if (Annotation.IsOperator || Annotations[i - 1].IsOperator) {
          Annotation.SpaceRequiredBefore = true;
        } else {
          Annotation.SpaceRequiredBefore =
              spaceRequiredBetween(Line.Tokens[i - 1].Tok, Line.Tokens[i].Tok);
        }
      }
      Annotations.push_back(Annotation);
    }
  }

  bool isBinaryOperator(const FormatToken &Tok) {
    switch(Tok.Tok.getKind()) {
    case tok::star:
    //case tok::amp:
    case tok::plus:
    case tok::slash:
    case tok::minus:
    case tok::ampamp:
    case tok::pipe:
    case tok::pipepipe:
    case tok::percent:
      return true;
    default:
      return false;
    }
  }

  bool canBreakBetween(const FormatToken &Left, const FormatToken &Right) {
    if (Right.Tok.is(tok::r_paren))
      return false;
    if (isBinaryOperator(Left))
      return true;
    return Right.Tok.is(tok::colon) ||
      Left.Tok.is(tok::comma) || Left.Tok.is(tok::semi) ||
      Left.Tok.is(tok::equal) || Left.Tok.is(tok::ampamp) ||
      (Left.Tok.is(tok::l_paren) && !Right.Tok.is(tok::r_paren));
  }

  typedef std::map<IndentState, unsigned> StateMap;
  StateMap Memory;

  unsigned bindPenalty(const FormatToken &Token) {
    if (Token.Tok.is(tok::semi))
      return 0;
    if (Token.Tok.is(tok::equal) || Token.Tok.is(tok::comma))
      return 1;
    return 2;
  }

  /// \brief Calculate the number of lines needed to format the remaining part
  /// of the unwrapped line.
  ///
  /// Assumes the formatting so far has led to
  /// the \c IndentState \p State. If \p NewLine is set, a new line will be
  /// added after the previous token.
  ///
  /// \param StopAt is used for optimization. If we can determine that we'll
  /// definitely need at least \p StopAt additional lines, we already know of a
  /// better solution.
  unsigned calcPenalty(IndentState State, bool NewLine, unsigned StopAt) {
    // We are at the end of the unwrapped line, so we don't need any more lines.
    if (State.ConsumedTokens >= Line.Tokens.size())
      return 0;

    if (NewLine && !Annotations[State.ConsumedTokens].CanBreakBefore)
      return UINT_MAX;

    if (State.ConsumedTokens > 0 && !NewLine &&
        State.CtorInitializerOnNewLine &&
        Line.Tokens[State.ConsumedTokens - 1].Tok.is(tok::comma))
      return UINT_MAX;

    if (NewLine && State.InCtorInitializer && !State.CtorInitializerOnNewLine)
      return UINT_MAX;

    addToken(NewLine, true, State);

    // Exceeding column limit is bad.
    if (State.Column > Style.ColumnLimit)
      return UINT_MAX;

    unsigned CurrentPenalty = 0;
    if (NewLine) {
      CurrentPenalty += Parameters.PenaltyIndentLevel *
          Annotations[State.ConsumedTokens - 1].ParenLevel +
          Parameters.PenaltyExtraLine +
          bindPenalty(Line.Tokens[State.ConsumedTokens - 2]);
    }

    if (StopAt <= CurrentPenalty)
      return UINT_MAX;
    StopAt -= CurrentPenalty;

    // Has this state already been examined?
    StateMap::iterator I = Memory.find(State);
    if (I != Memory.end())
      return I->second;
    ++count;

    unsigned NoBreak = calcPenalty(State, false, StopAt);
    unsigned WithBreak = calcPenalty(State, true, std::min(StopAt, NoBreak));
    unsigned Result = std::min(NoBreak, WithBreak);
    if (Result != UINT_MAX)
      Result += CurrentPenalty;
    Memory[State] = Result;
    assert(Memory.find(State) != Memory.end());
    return Result;
  }

  /// \brief Replaces the whitespace in front of \p Tok. Only call once for
  /// each \c FormatToken.
  void replaceWhitespace(const FormatToken &Tok, unsigned NewLines,
                         unsigned Spaces) {
    Replaces.insert(tooling::Replacement(
        SourceMgr, Tok.WhiteSpaceStart, Tok.WhiteSpaceLength,
        std::string(NewLines, '\n') + std::string(Spaces, ' ')));
  }

  bool isIfForOrWhile(Token Tok) {
    if (Tok.getKind() != tok::raw_identifier)
      return false;
    StringRef Data(SourceMgr.getCharacterData(Tok.getLocation()),
                   Tok.getLength());
    return Data == "for" || Data == "while" || Data == "if";
  }

  bool spaceRequiredBetween(Token Left, Token Right) {
    if (Left.is(tok::greater) && Right.is(tok::greater))
      return true;
    if (Left.is(tok::less) && Right.is(tok::less))
      return true;
    if (Left.is(tok::arrow) || Right.is(tok::arrow))
      return false;
    if (Left.is(tok::exclaim))
      return false;
    if (Left.is(tok::less) || Right.is(tok::greater) || Right.is(tok::less))
      return false;
    if (Left.is(tok::amp) || Left.is(tok::star))
      return Right.isLiteral() || Style.PointerAndReferenceBindToType;
    if (Right.is(tok::amp) || Right.is(tok::star))
      return Left.isLiteral() || !Style.PointerAndReferenceBindToType;
    if (Left.is(tok::l_square) || Right.is(tok::l_square) ||
        Right.is(tok::r_square))
      return false;
    if (Left.is(tok::coloncolon) || Right.is(tok::coloncolon))
      return false;
    if (Left.is(tok::period) || Right.is(tok::period))
      return false;
    if (Left.is(tok::colon) || Right.is(tok::colon))
      return true;
    if ((Left.is(tok::plusplus) && Right.is(tok::raw_identifier)) ||
        (Left.is(tok::raw_identifier) && Right.is(tok::plusplus)) ||
        (Left.is(tok::minusminus) && Right.is(tok::raw_identifier)) ||
        (Left.is(tok::raw_identifier) && Right.is(tok::minusminus)))
      return false;
    if (Left.is(tok::l_paren))
      return false;
    if (Left.is(tok::hash))
      return false;
    if (Right.is(tok::r_paren) || Right.is(tok::semi) || Right.is(tok::comma))
      return false;
    if (Right.is(tok::l_paren)) {
      return !Left.is(tok::raw_identifier) || isIfForOrWhile(Left);
    }
    return true;
  }

  /// \brief Add a new line and the required indent before \p Token.
  void addNewline(const FormatToken &Token, unsigned Level, unsigned Num = 1) {
    if (Token.WhiteSpaceStart.isValid()) {
      unsigned Newlines =
          std::min(Token.NewlinesBefore, Style.MaxEmptyLinesToKeep + 1);
      unsigned Offset = SourceMgr.getFileOffset(Token.WhiteSpaceStart);
      if (Newlines == 0 && Offset != 0)
        Newlines = 1;
      unsigned Indent = Level * 2;
      if (Token.Tok.is(tok::raw_identifier)) {
        StringRef Text(SourceMgr.getCharacterData(Token.Tok.getLocation()),
                       Token.Tok.getLength());
        if (Text == "public" || Text == "protected" || Text == "private")
          Indent += Style.AccessModifierOffset;
      }
      replaceWhitespace(Token, Newlines, Indent);
    }
  }

  FormatStyle Style;
  SourceManager &SourceMgr;
  const UnwrappedLine &Line;
  std::vector<TokenAnnotation> Annotations;
  tooling::Replacements &Replaces;
  unsigned int count;

  OptimizationParameters Parameters;
};

class Formatter : public UnwrappedLineConsumer {
public:
  Formatter(const FormatStyle &Style, Lexer &Lex, SourceManager &SourceMgr,
            const std::vector<CodeRange> &Ranges)
      : Style(Style),
        Lex(Lex),
        SourceMgr(SourceMgr),
        Ranges(Ranges) {
  }

  tooling::Replacements format() {
    UnwrappedLineParser Parser(Lex, SourceMgr, *this);
    Parser.parse();
    return Replaces;
  }

private:
  virtual void formatUnwrappedLine(const UnwrappedLine &TheLine) {
    if (TheLine.Tokens.size() == 0)
      return;

    unsigned LineBegin =
        SourceMgr.getFileOffset(TheLine.Tokens.front().Tok.getLocation());
    // FIXME: Add length of last token.
    unsigned LineEnd =
        SourceMgr.getFileOffset(TheLine.Tokens.back().Tok.getLocation());

    for (unsigned i = 0, e = Ranges.size(); i != e; ++i) {
      unsigned RangeBegin = Ranges[i].Offset;
      unsigned RangeEnd = RangeBegin + Ranges[i].Length;
      if (LineEnd < RangeBegin || LineBegin > RangeEnd)
        continue;

      UnwrappedLineFormatter Formatter(Style, SourceMgr, TheLine, Replaces);
      Formatter.format();
      return;
    }
  }

  FormatStyle Style;
  Lexer &Lex;
  SourceManager &SourceMgr;
  tooling::Replacements Replaces;
  std::vector<CodeRange> Ranges;
};

tooling::Replacements reformat(const FormatStyle &Style, Lexer &Lex,
                               SourceManager &SourceMgr,
                               std::vector<CodeRange> Ranges) {
  Formatter formatter(Style, Lex, SourceMgr, Ranges);
  return formatter.format();
}

}  // namespace format
}  // namespace clang
