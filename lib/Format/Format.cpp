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

class UnwrappedLineFormatter {
public:
  UnwrappedLineFormatter(SourceManager &SourceMgr,
                         const UnwrappedLine &Line,
                         tooling::Replacements &Replaces)
      : SourceMgr(SourceMgr), Line(Line), Replaces(Replaces) {}

  void format() {
    addNewline(Line.Tokens[0], Line.Level);
    count = 0;
    IndentState State;
    State.ParenLevel = 0;
    State.Column = Line.Level * 2 + Line.Tokens[0].Tok.getLength();

    State.UsedIndent.push_back(Line.Level * 2);
    State.Indent.push_back(Line.Level * 2 + 4);

    // Start iterating at 1 as we have correctly formatted of Token #0 above.
    for (unsigned i = 1, n = Line.Tokens.size(); i != n; ++i) {
      //bool InsertNewLine = Line.Tokens[i].NewlinesBefore > 0;
      bool InsertNewLine = false;
      if (!InsertNewLine) {
        unsigned NoBreak = numLines(State, false, i + 1,
                                    Line.Tokens.size(), 100000);
        unsigned Break = numLines(State, true, i + 1,
                                  Line.Tokens.size(), 100000);
        InsertNewLine = Break < NoBreak;
      }
      addToken(i, InsertNewLine, false, State);
    }
  }

private:
  /// \brief The current state when indenting a unwrapped line.
  ///
  /// As the indenting tries different combinations this is copied by value.
  struct IndentState {
    /// \brief The current parenthesis level, i.e. the number of opening minus
    /// the number of closing parenthesis left of the current position.
    unsigned ParenLevel;

    /// \brief The number of used columns in the current line.
    unsigned Column;

    /// \brief The position to which a specific parenthesis level needs to be
    /// indented.
    std::vector<unsigned> Indent;

    /// \brief The indents actively used by a parenthesis level.
    ///
    /// This is used to prevent situations like:
    /// \code
    ///   callA(callB(
    ///       callC()),
    ///         callD()).
    /// \endcode
    /// We might (configurably) not want callC() to be indented less callD()
    /// as it has a higher indent level.
    std::vector<unsigned> UsedIndent;
  };

  /// Append the token at \p Index to \p State.
  void addToken(unsigned Index, bool Newline, bool DryRun, IndentState &State) {
    if (Line.Tokens[Index].Tok.getKind() == tok::l_paren) {
      State.UsedIndent.push_back(State.UsedIndent.back());
      State.Indent.push_back(State.UsedIndent.back() + 4);
      ++State.ParenLevel;
    }
    if (Newline) {
      if (!DryRun)
        replaceWhitespace(Line.Tokens[Index], 1,
                          State.Indent[State.ParenLevel]);
      State.Column = State.Indent[State.ParenLevel] +
          Line.Tokens[Index].Tok.getLength();
      State.UsedIndent[State.ParenLevel] = State.Indent[State.ParenLevel];
    } else {
      bool Space = spaceRequiredBetween(Line.Tokens[Index - 1].Tok,
                                        Line.Tokens[Index].Tok);
      //if (Line.Tokens[Index].NewlinesBefore == 0)
      //  Space = Line.Tokens[Index].WhiteSpaceLength > 0;
      if (!DryRun)
        replaceWhitespace(Line.Tokens[Index], 0, Space ? 1 : 0);
      if (Line.Tokens[Index - 1].Tok.getKind() == tok::l_paren)
        State.Indent[State.ParenLevel] = State.Column;
      State.Column += Line.Tokens[Index].Tok.getLength() + (Space ? 1 : 0);
    }

    if (Line.Tokens[Index].Tok.getKind() == tok::r_paren) {
      // FIXME: We should be able to handle this kind of code.
      assert(State.ParenLevel != 0 && "Unexpected ')'.");
      --State.ParenLevel;
      State.Indent.pop_back();
    }
  }

  bool canBreakAfter(Token tok) {
    return tok.getKind() == tok::comma || tok.getKind() == tok::semi ||
        tok.getKind() == tok::l_paren;
  }

  /// \brief Calculate the number of lines needed to format the remaining part
  /// of the unwrapped line.
  ///
  /// Assumes the formatting of the \c Token until \p EndIndex has led to
  /// the \c IndentState \p State. If \p NewLine is set, a new line will be
  /// added after the previous token.
  ///
  /// \param EndIndex is the last token belonging to the unwrapped line.
  ///
  /// \param StopAt is used for optimization. If we can determine that we'll
  /// definitely need at least \p StopAt additional lines, we already know of a
  /// better solution.
  unsigned numLines(IndentState State, bool NewLine, unsigned Index,
                    unsigned EndIndex, unsigned StopAt) {
    count++;

    // We are at the end of the unwrapped line, so we don't need any more lines.
    if (Index > EndIndex)
      return 0;

    addToken(Index - 1, NewLine, true, State);
    if (NewLine)
      --StopAt;

    // Exceeding 80 columns is bad.
    if (State.Column > 80)
      return 10000;

    if (StopAt < 1)
      return 10000;

    unsigned NoBreak = numLines(State, false, Index + 1, EndIndex, StopAt);
    if (!canBreakAfter(Line.Tokens[Index - 1].Tok))
      return NoBreak + (NewLine ? 1 : 0);
    unsigned Break = numLines(State, true, Index + 1, EndIndex,
                         std::min(StopAt, NoBreak));
    return std::min(NoBreak, Break) + (NewLine ? 1 : 0);
  }

  /// \brief Replaces the whitespace in front of \p Tok. Only call once for
  /// each \c FormatToken.
  void replaceWhitespace(const FormatToken &Tok, unsigned NewLines,
                         unsigned Spaces) {
    Replaces.insert(tooling::Replacement(SourceMgr, Tok.WhiteSpaceStart,
                                         Tok.WhiteSpaceLength,
                                         std::string(NewLines, '\n') +
                                         std::string(Spaces, ' ')));
  }

  bool isIfForOrWhile(Token Tok) {
    if (Tok.getKind() != tok::raw_identifier)
      return false;
    StringRef Data(SourceMgr.getCharacterData(Tok.getLocation()),
                   Tok.getLength());
    return Data == "for" || Data == "while" || Data == "if";
  }

  bool spaceRequiredBetween(Token Left, Token Right) {
    if (Left.is(tok::exclaim))
      return false;
    if (Left.is(tok::less) || Right.is(tok::greater) || Right.is(tok::less))
      return false;
    if (Left.is(tok::amp) || Left.is(tok::star))
      return false;
    if (Left.is(tok::l_square) || Right.is(tok::l_square) ||
        Right.is(tok::r_square))
      return false;
    if (Left.is(tok::period) || Right.is(tok::period))
      return false;
    if (Left.is(tok::colon) || Right.is(tok::colon))
      return false;
    if (Left.is(tok::plusplus) && Right.is(tok::raw_identifier))
      return false;
    if (Left.is(tok::l_paren))
      return false;
    if (Right.is(tok::r_paren) || Right.is(tok::semi) || Right.is(tok::comma))
      return false;
    if (Right.is(tok::l_paren)) {
      return isIfForOrWhile(Left);
    }
    return true;
  }

  /// \brief Add a new line and the required indent before \p Token.
  void addNewline(const FormatToken &Token, unsigned Level) {
      //unsigned Index, unsigned Level) {
    if (Token.WhiteSpaceStart.isValid()) {
      unsigned Newlines = Token.NewlinesBefore;
      unsigned Offset = SourceMgr.getFileOffset(Token.WhiteSpaceStart);
      if (Newlines == 0 && Offset != 0)
        Newlines = 1;
      replaceWhitespace(Token, Newlines, Level * 2);
    }
  }

  SourceManager &SourceMgr;
  const UnwrappedLine &Line;
  tooling::Replacements &Replaces;
  unsigned int count;
};

class Formatter : public UnwrappedLineConsumer {
public:
  Formatter(Lexer &Lex, SourceManager &SourceMgr,
            const std::vector<CodeRange> &Ranges)
      : Lex(Lex), SourceMgr(SourceMgr), Ranges(Ranges) {}

  tooling::Replacements format() {
    UnwrappedLineParser Parser(Lex, SourceMgr, *this);
    Parser.parse();
    return Replaces;
  }

private:
  virtual void formatUnwrappedLine(const UnwrappedLine &TheLine) {
    if (TheLine.Tokens.size() == 0)
      return;

    unsigned LineBegin = SourceMgr.getFileOffset(
        TheLine.Tokens.front().Tok.getLocation());
    // FIXME: Add length of last token.
    unsigned LineEnd = SourceMgr.getFileOffset(
        TheLine.Tokens.back().Tok.getLocation());

    for (unsigned i = 0, e = Ranges.size(); i != e; ++i) {
      unsigned RangeBegin = Ranges[i].Offset;
      unsigned RangeEnd = RangeBegin + Ranges[i].Length;
      if (LineEnd < RangeBegin || LineBegin > RangeEnd)
        continue;

      UnwrappedLineFormatter Formatter(SourceMgr, TheLine, Replaces);
      Formatter.format();
      return;
    }
  }

  Lexer &Lex;
  SourceManager &SourceMgr;
  tooling::Replacements Replaces;
  std::vector<CodeRange> Ranges;
};

tooling::Replacements reformat(Lexer &Lex, SourceManager &SourceMgr,
                               std::vector<CodeRange> Ranges) {
  Formatter formatter(Lex, SourceMgr, Ranges);
  return formatter.format();
}

}  // namespace format
}  // namespace clang
