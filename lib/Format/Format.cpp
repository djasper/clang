//===--- Format.cpp - Format C++ code -------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This is EXPERIMENTAL code under heavy development. It is not in a state yet,
//  where it can be used to format real code.
//
//  Implements Format.h.
//
//===----------------------------------------------------------------------===//

#include "clang/Format/Format.h"

#include "clang/Basic/SourceManager.h"
#include "clang/Lex/Lexer.h"

#include "ContinuationParser.h"

namespace clang {
namespace format {

using llvm::MutableArrayRef;

class ContinuationFormatter {
public:
  ContinuationFormatter(SourceManager &Sources,
                        const Continuation &Cont,
                        tooling::Replacements &Replaces)
      : Sources(Sources), Cont(Cont), Replaces(Replaces) {}

  void format() {
    addNewline(Cont.Tokens[0], Cont.Level);
    count = 0;
    IndentState State;
    State.ParenLevel = 0;
    State.Column = Cont.Level * 2 + Cont.Tokens[0].Tok.getLength();

    State.UsedIndent.push_back(Cont.Level * 2);
    State.Indent.push_back(Cont.Level * 2 + 4);
    for (unsigned i = 1; i < Cont.Tokens.size(); ++i) {
      bool InsertNewLine = Cont.Tokens[i].NewlinesBefore > 0;
      if (!InsertNewLine) {
        int NoBreak = numLines(State, false, i + 1,
                               Cont.Tokens.size()-1, 100000);
        int Break = numLines(State, true, i + 1, Cont.Tokens.size()-1, 100000);
        InsertNewLine = Break < NoBreak;
      }
      addToken(i, InsertNewLine, false, State);
    }
  }

private:
  // The current state when indenting a continuation.
  struct IndentState {
    unsigned ParenLevel;
    unsigned Column;
    std::vector<unsigned> Indent;
    std::vector<unsigned> UsedIndent;
  };

  // Append the token at 'Index' to the IndentState 'State'.
  void addToken(unsigned Index, bool Newline, bool DryRun, IndentState &State) {
    if (Cont.Tokens[Index].Tok.getKind() == tok::l_paren) {
      State.UsedIndent.push_back(State.UsedIndent.back());
      State.Indent.push_back(State.UsedIndent.back() + 4);
      ++State.ParenLevel;
    }
    if (Newline) {
      if (!DryRun)
        setWhitespace(Cont.Tokens[Index], 1, State.Indent[State.ParenLevel]);
      State.Column = State.Indent[State.ParenLevel] +
          Cont.Tokens[Index].Tok.getLength();
      State.UsedIndent[State.ParenLevel] = State.Indent[State.ParenLevel];
    } else {
      bool Space = spaceRequiredBetween(Cont.Tokens[Index - 1].Tok,
                                        Cont.Tokens[Index].Tok);
      //if (Cont.Tokens[Index].NewlinesBefore == 0)
      //  Space = Cont.Tokens[Index].WhiteSpaceLength > 0;
      if (!DryRun)
        setWhitespace(Cont.Tokens[Index], 0, Space ? 1 : 0);
      if (Cont.Tokens[Index - 1].Tok.getKind() == tok::l_paren)
        State.Indent[State.ParenLevel] = State.Column;
      State.Column += Cont.Tokens[Index].Tok.getLength() + (Space ? 1 : 0);
    }

    if (Cont.Tokens[Index].Tok.getKind() == tok::r_paren) {
      if (State.ParenLevel == 0) {
        llvm::outs() << "ParenLevel is 0!!!\n";
        abort();
      }
      --State.ParenLevel;
      State.Indent.pop_back();
    }
  }

  bool canBreakAfter(Token tok) {
    return tok.getKind() == tok::comma || tok.getKind() == tok::semi ||
        tok.getKind() == tok::l_paren;
  }

  // Calculate the number of lines needed to format the remaining part of the
  // continuation starting in the state 'State'. If 'NewLine' is set, a new line
  // will be added after the previous token.
  // 'EndIndex' is the last token belonging to the continuation.
  // 'StopAt' is used for optimization. If we can determine that we'll
  // definitely need more than 'StopAt' additional lines, we already know of a
  // better solution.
  int numLines(IndentState State, bool NewLine, unsigned Index,
               unsigned EndIndex, int StopAt) {
    count++;

    // We are at the end of the continuation, so we don't need any more lines.
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

    int NoBreak = numLines(State, false, Index + 1, EndIndex, StopAt);
    if (!canBreakAfter(Cont.Tokens[Index - 1].Tok))
      return NoBreak + (NewLine ? 1 : 0);
    int Break = numLines(State, true, Index + 1, EndIndex,
                         std::min(StopAt, NoBreak));
    return std::min(NoBreak, Break) + (NewLine ? 1 : 0);
  }

  void setWhitespace(const FormatToken& Tok, unsigned NewLines,
                     unsigned Spaces) {
    Replaces.insert(tooling::Replacement(Sources, Tok.WhiteSpaceStart,
                                         Tok.WhiteSpaceLength,
                                         std::string(NewLines, '\n') +
                                         std::string(Spaces, ' ')));
  }

  bool isIfForOrWhile(Token Tok) {
    if (Tok.getKind() != tok::raw_identifier)
      return false;
    StringRef Data(Sources.getCharacterData(Tok.getLocation()),
        Tok.getLength());
    return Data == "for" || Data == "while" || Data == "if";
  }

  bool spaceRequiredBetween(Token Left, Token Right) {

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

  /// \brief Add a new line before token \c Index.
  void addNewline(const FormatToken &Token, unsigned Level) {
      //unsigned Index, unsigned Level) {
    if (Token.WhiteSpaceStart.isValid()) {
      unsigned Newlines = Token.NewlinesBefore;
      unsigned Offset = Sources.getFileOffset(Token.WhiteSpaceStart);
      if (Newlines == 0 && Offset != 0)
        Newlines = 1;
      setWhitespace(Token, Newlines, Level * 2);
    }
  }

  SourceManager &Sources;
  const Continuation &Cont;
  tooling::Replacements &Replaces;
  unsigned int count;
};

class Formatter : public ContinuationConsumer {
public:
  Formatter(Lexer &Lex, SourceManager &Sources,
            const std::vector<CodeRange> &Ranges)
      : Lex(Lex), Sources(Sources) {}

  tooling::Replacements format() {
    ContinuationParser Parser(Lex, Sources, *this);
    Parser.parse();
    return Replaces;
  }

private:
  virtual void formatContinuation(const Continuation &TheCont) {
    ContinuationFormatter Formatter(Sources, TheCont, Replaces);
    Formatter.format();
  }

  Lexer &Lex;
  SourceManager &Sources;
  tooling::Replacements Replaces;
};

tooling::Replacements reformat(Lexer &Lex, SourceManager &Sources,
                               std::vector<CodeRange> Ranges) {
  Formatter formatter(Lex, Sources, Ranges);
  return formatter.format();
}

}  // namespace format
}  // namespace clang
