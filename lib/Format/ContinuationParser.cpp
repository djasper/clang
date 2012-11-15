//===--- ContinuationParser.cpp - Format C++ code -------------------------===//
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
//===----------------------------------------------------------------------===//

#include "ContinuationParser.h"

#include "llvm/Support/raw_ostream.h"

namespace clang {
namespace format {

ContinuationParser::ContinuationParser(Lexer &Lex, SourceManager &SourceMgr,
                                       ContinuationConsumer &Callback)
    : Lex(Lex), SourceMgr(SourceMgr), Callback(Callback) {
  Lex.SetKeepWhitespaceMode(true);
}

void ContinuationParser::parse() {
  parseToken();
  parseLevel();
}

void ContinuationParser::parseLevel() {
  do {
    switch(FormatTok.Tok.getKind()) {
      case tok::hash:
        parsePPDirective();
        break;
      case tok::comment:
        parseComment();
        break;
      case tok::l_brace:
        parseBlock();
        addContinuation();
        break;
      case tok::r_brace:
        return;
      default:
        parseStatement();
        break;
    }
  } while (!eof());
}

void ContinuationParser::parseBlock() {
  nextToken();
  addContinuation();
  ++Cont.Level;
  parseLevel();
  --Cont.Level;
  if (FormatTok.Tok.getKind() != tok::r_brace) abort();
  nextToken();
  if (FormatTok.Tok.getKind() == tok::semi)
    nextToken();
}

void ContinuationParser::parsePPDirective() {
  while (!eof()) {
    nextToken();
    if (FormatTok.NewlinesBefore > 0) return;
  }
}

void ContinuationParser::parseComment() {
  while (!eof()) {
    nextToken();
    if (FormatTok.NewlinesBefore > 0) {
      addContinuation();
      return;
    }
  }
}

void ContinuationParser::parseStatement() {
  do {
    switch (FormatTok.Tok.getKind()) {
      case tok::semi:
        nextToken();
        addContinuation();
        return;
      case tok::l_paren:
        parseParens();
        break;
      case tok::l_brace:
        parseBlock();
        addContinuation();
        return;
      case tok::raw_identifier:
        if (tokenText() == "if") {
          parseIfThenElse();
          return;
        }
      default:
        nextToken();
        break;
    }
  } while (!eof());
} 

void ContinuationParser::parseParens() {
  assert(FormatTok.Tok.getKind() == tok::l_paren && "'(' expected.");
  nextToken();
  do {
    switch (FormatTok.Tok.getKind()) {
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

void ContinuationParser::parseIfThenElse() {
  assert(FormatTok.Tok.getKind() == tok::raw_identifier &&
         "Identifier expected");
  nextToken();
  parseParens();
  bool NeedsContinuation = false;
  if (FormatTok.Tok.getKind() == tok::l_brace) {
    parseBlock();
    NeedsContinuation = true;
  } else {
    addContinuation();
    ++Cont.Level;
    parseStatement();
    --Cont.Level;
  }
  if (FormatTok.Tok.is(tok::raw_identifier) && tokenText() == "else") {
    nextToken();
    if (FormatTok.Tok.getKind() == tok::l_brace) {
      parseBlock();
      addContinuation();
    } else {
      addContinuation();
      ++Cont.Level;
      parseStatement();
      --Cont.Level;
    }
  } else if (NeedsContinuation) {
    addContinuation();
  }
}

void ContinuationParser::addContinuation() {
  Callback.formatContinuation(Cont);
  Cont.Tokens.clear();
}

bool ContinuationParser::eof() const {
  return FormatTok.Tok.getKind() == tok::eof;
}

void ContinuationParser::nextToken() {
  if (eof())
    return;
  Cont.Tokens.push_back(FormatTok);
  parseToken();
}

void ContinuationParser::parseToken() {
  FormatTok = FormatToken();
  Lex.LexFromRawLexer(FormatTok.Tok);
  FormatTok.WhiteSpaceStart = FormatTok.Tok.getLocation();

  // Consume and record whitespace until we find a significant
  // token.
  while (FormatTok.Tok.getKind() == tok::unknown) {
    StringRef Data = tokenText();
    if (std::find(Data.begin(), Data.end(), '\n') != Data.end())
      ++FormatTok.NewlinesBefore;
    FormatTok.WhiteSpaceLength += FormatTok.Tok.getLength();

    if (eof()) return;
    Lex.LexFromRawLexer(FormatTok.Tok);
  }
}

StringRef ContinuationParser::tokenText() {
  StringRef Data(SourceMgr.getCharacterData(FormatTok.Tok.getLocation()),
                 FormatTok.Tok.getLength());
  return Data;
}

} // end namespace format
} // end namespace clang
