//===--- UnwrappedLineParser.cpp - Format C++ code ------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief This file contains the implementation of the UnwrappedLineParser,
/// which turns a stream of tokens into UnwrappedLines.
///
/// This is EXPERIMENTAL code under heavy development. It is not in a state yet,
/// where it can be used to format real code.
///
//===----------------------------------------------------------------------===//

#include "UnwrappedLineParser.h"

#include "llvm/Support/raw_ostream.h"

namespace clang {
namespace format {

UnwrappedLineParser::UnwrappedLineParser(Lexer &Lex, SourceManager &SourceMgr,
                                         UnwrappedLineConsumer &Callback)
    : Lex(Lex), SourceMgr(SourceMgr), Callback(Callback) {
  Lex.SetKeepWhitespaceMode(true);
}

void UnwrappedLineParser::parse() {
  parseToken();
  parseLevel();
}

void UnwrappedLineParser::parseLevel() {
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
        addUnwrappedLine();
        break;
      case tok::r_brace:
        return;
      default:
        parseStatement();
        break;
    }
  } while (!eof());
}

void UnwrappedLineParser::parseBlock() {
  nextToken();

  // FIXME: Remove this hack to handle namespaces.
  bool IsNamespace = false;
  if (Line.Tokens.size() > 0) {  
    StringRef Data(SourceMgr.getCharacterData(Line.Tokens[0].Tok.getLocation()),
        Line.Tokens[0].Tok.getLength());
    IsNamespace = Data == "namespace";
  }

  addUnwrappedLine();

  if (!IsNamespace)
    ++Line.Level;
  parseLevel();
  if (!IsNamespace)
    --Line.Level;
  assert(FormatTok.Tok.is(tok::r_brace) && "expected '}'");
  nextToken();
  if (FormatTok.Tok.getKind() == tok::semi)
    nextToken();
}

void UnwrappedLineParser::parsePPDirective() {
  while (!eof()) {
    nextToken();
    if (FormatTok.NewlinesBefore > 0) {
      addUnwrappedLine();
      return;
    }
  }
}

void UnwrappedLineParser::parseComment() {
  while (!eof()) {
    nextToken();
    if (FormatTok.NewlinesBefore > 0) {
      addUnwrappedLine();
      return;
    }
  }
}

void UnwrappedLineParser::parseStatement() {
  do {
    switch (FormatTok.Tok.getKind()) {
      case tok::semi:
        nextToken();
        addUnwrappedLine();
        return;
      case tok::l_paren:
        parseParens();
        break;
      case tok::l_brace:
        parseBlock();
        addUnwrappedLine();
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

void UnwrappedLineParser::parseParens() {
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

void UnwrappedLineParser::parseIfThenElse() {
  assert(FormatTok.Tok.getKind() == tok::raw_identifier &&
         "Identifier expected");
  nextToken();
  parseParens();
  bool NeedsUnwrappedLine = false;
  if (FormatTok.Tok.getKind() == tok::l_brace) {
    parseBlock();
    NeedsUnwrappedLine = true;
  } else {
    addUnwrappedLine();
    ++Line.Level;
    parseStatement();
    --Line.Level;
  }
  if (FormatTok.Tok.is(tok::raw_identifier) && tokenText() == "else") {
    nextToken();
    if (FormatTok.Tok.getKind() == tok::l_brace) {
      parseBlock();
      addUnwrappedLine();
    } else {
      addUnwrappedLine();
      ++Line.Level;
      parseStatement();
      --Line.Level;
    }
  } else if (NeedsUnwrappedLine) {
    addUnwrappedLine();
  }
}

void UnwrappedLineParser::addUnwrappedLine() {
  Callback.formatUnwrappedLine(Line);
  Line.Tokens.clear();
}

bool UnwrappedLineParser::eof() const {
  return FormatTok.Tok.getKind() == tok::eof;
}

void UnwrappedLineParser::nextToken() {
  if (eof())
    return;
  Line.Tokens.push_back(FormatTok);
  parseToken();
}

void UnwrappedLineParser::parseToken() {
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

    if (eof())
      return;
    Lex.LexFromRawLexer(FormatTok.Tok);
  }
}

StringRef UnwrappedLineParser::tokenText() {
  StringRef Data(SourceMgr.getCharacterData(FormatTok.Tok.getLocation()),
                 FormatTok.Tok.getLength());
  return Data;
}

} // end namespace format
} // end namespace clang
