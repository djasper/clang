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

class Formatter : public ContinuationConsumer {
public:
  Formatter(Lexer &Lex, SourceManager &Sources,
            const std::vector<CodeRange> &Ranges)
      : Lex(Lex), Sources(Sources), EndOfFile(false) {}

  tooling::Replacements format() {
    ContinuationParser Parser(Lex, Sources, *this);
    Parser.parse();
    return Replaces;
    /*

    Lex.SetKeepWhitespaceMode(true);

    FormatToken NextToken;
    NextToken.WhiteSpaceLength = 0;

    // Read token stream and turn tokens into FormatTokens.
    while (!EndOfFile) {
      NextToken.Tok = getNextToken();
      StringRef Data(Sources.getCharacterData(NextToken.Tok.getLocation()),
          NextToken.Tok.getLength());
      if (NextToken.WhiteSpaceLength == 0) {
        NextToken.WhiteSpaceStart = NextToken.Tok.getLocation();
        NextToken.NewlinesBefore = 0;
      }
      if (NextToken.Tok.getKind() == tok::unknown) {
        StringRef Data(Sources.getCharacterData(NextToken.Tok.getLocation()),
                       NextToken.Tok.getLength());
        if (std::find(Data.begin(), Data.end(), '\n') != Data.end())
          ++NextToken.NewlinesBefore;
        NextToken.WhiteSpaceLength += NextToken.Tok.getLength();
        continue;
      }
      Tokens.push_back(NextToken);
      NextToken.WhiteSpaceLength = 0;
    }

    splitAndFormatContinuations();

    return Replaces;
    */
  }

private:
  FormatToken Tok;
  size_t Index;
  int Level;
  bool nextToken() {
    if (Index == Tokens.size())
      return false;
    ++Index;
    if (Index == Tokens.size())
      return false;
    Tok = Tokens[Index];
    return true;
  }

  bool eof() {
    return Index == Tokens.size();
  }

  void parseLevel() {
    do {
      switch(Tok.Tok.getKind()) {
        case tok::hash:
          parsePPDirective();
          break;
        case tok::comment:
          parseComment();
          break;
        case tok::l_brace:
          addContinuation(Index, Index, Level);
          parseBlock();
          break;
        case tok::r_brace:
          return;
        default:
          parseStatement();
          break;
      }
    } while (!eof());
  }

  void parseBlock() {
    if (!nextToken()) return;
    ++Level;
    parseLevel();
    --Level;
    if (Tok.Tok.getKind() != tok::r_brace) abort();
    addContinuation(Index, Index, Level);
    nextToken();
    if (!eof() && Tok.Tok.getKind() == tok::semi)
      nextToken();
  }

  void parsePPDirective() {
    while (nextToken() ) {
      if (Tok.NewlinesBefore > 0) return;
    }
  }

  void parseComment() {
    size_t Start = Index;
    while (nextToken()) {
      if (Tok.NewlinesBefore > 0) {
        addContinuation(Start, Index - 1, Level);
        return;
      }
    }
  }

  void parseStatement() {
    size_t Start = Index;
    do {
      switch (Tok.Tok.getKind()) {
        case tok::semi:
          {
            size_t End = Index;
            addContinuation(Start, End, Level);
            nextToken();
            return;
          }
        case tok::l_paren:
          parseParens();
          break;
        case tok::l_brace:
          {
            size_t End = Index;
            addContinuation(Start, End, 0);
            parseBlock(  ); // TODO: Test
            return;
          }
        case tok::raw_identifier:
          {
            StringRef Data(Sources.getCharacterData(Tok.Tok.getLocation()),
                           Tok.Tok.getLength());
            if (Data == "if") {
              parseIfThenElse();
              return;
            }
          }
        default:
          nextToken();
          break;
      }
    } while (!eof());
  }

  void parseParens() {
    if (Tok.Tok.getKind() != tok::l_paren) abort();
    nextToken();
    do {
      switch (Tok.Tok.getKind()) {
        case tok::l_paren:
          parseParens();
          break;
        case tok::r_paren:
          nextToken();
          return;
        default:
          nextToken();
          break;
      }
    } while (!eof());
  }

  void parseIfThenElse() {
    size_t Start = Index;
    if (Tok.Tok.getKind() != tok::raw_identifier) abort();
    if (!nextToken()) return;
    parseParens();
    if (Tok.Tok.getKind() == tok::l_brace) {
      addContinuation(Start, Index, Level);
      parseBlock(); // TODO: Level Test
    } else {
      addContinuation(Start, Index - 1, Level);
      ++Level;
      parseStatement();
      --Level;
    }
    //if (!nextToken()) return;
    if (Tok.Tok.getKind() == tok::raw_identifier) {
      StringRef Data(Sources.getCharacterData(Tok.Tok.getLocation()),
                     Tok.Tok.getLength());
      if (Data == "else") {
        if (!nextToken()) return;
        parseStatement();
      }
    }
  }

  void addContinuation(unsigned Start, unsigned End, unsigned Level) {
    Continuation Cont;
    Cont.Tokens = ArrayRef<FormatToken>(Tokens).slice(Start, End+1-Start);
    Cont.Level = Level;
    formatContinuation(Cont);
  }

  /// \brief Split token stream into continuations, i.e. something that we'd
  /// on a single line if we didn't have a column limit.
  void splitAndFormatContinuations() {
    Index = 0;
    Tok = Tokens[Index];
    Level = 0;
    parseLevel();
  }

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

  Continuation Cont;

  virtual void formatContinuation(const Continuation &TheCont) {
    Cont = TheCont;
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
        int NoBreak = numLines(State, false, i + 1, Cont.Tokens.size()-1, 100000);
        int Break = numLines(State, true, i + 1, Cont.Tokens.size()-1, 100000);
        InsertNewLine = Break < NoBreak;
      }
      addToken(i, InsertNewLine, false, State);
    }
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

  Token getNextToken() {
    Token tok;
    EndOfFile = Lex.LexFromRawLexer(tok);
    return tok;
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

  Lexer &Lex;
  SourceManager &Sources;
  bool EndOfFile;
  tooling::Replacements Replaces;
  std::vector<FormatToken> Tokens;

  // Count number of tried states visited when formatting a continuation.
  unsigned int count;
};

tooling::Replacements reformat(Lexer &Lex, SourceManager &Sources,
                               std::vector<CodeRange> Ranges) {
  Formatter formatter(Lex, Sources, Ranges);
  return formatter.format();
}

}  // namespace format
}  // namespace clang
