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
    : Lex(Lex),
      SourceMgr(SourceMgr),
      Callback(Callback) {
  Lex.SetKeepWhitespaceMode(true);
}

void UnwrappedLineParser::parse() {
  parseToken();
  parseLevel();
}

void UnwrappedLineParser::parseLevel() {
  do {
    switch (FormatTok.Tok.getKind()) {
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
  if (FormatTok.Tok.is(tok::semi))
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
  StringRef Text = tokenText();
  if (Text == "public" || Text == "protected" || Text == "private") {
    parseAccessSpecifier();
    return;
  }
  if (Text == "enum") {
    parseEnum();
    return;
  }
  int TokenNumber = 0;
  do {
    ++TokenNumber;
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
      Text = tokenText();
      if (Text == "if") {
        parseIfThenElse();
        return;
      }
      if (Text == "switch") {
        parseSwitch();
        return;
      }
      if (Text == "default") {
        nextToken();
        parseLabel();
        return;
      }
      if (Text == "case") {
        parseCaseLabel();
        return;
      }
      nextToken();
      if (TokenNumber == 1 && FormatTok.Tok.is(tok::colon)) {
        parseLabel();
        return;
      }
      break;
    default:
      nextToken();
      break;
    }
  } while (!eof());
}

void UnwrappedLineParser::parseParens() {
  assert(FormatTok.Tok.is(tok::l_paren) && "'(' expected.");
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
  assert(FormatTok.Tok.is(tok::raw_identifier) && "Identifier expected");
  nextToken();
  parseParens();
  bool NeedsUnwrappedLine = false;
  if (FormatTok.Tok.is(tok::l_brace)) {
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
    if (FormatTok.Tok.is(tok::l_brace)) {
      parseBlock();
      addUnwrappedLine();
    } else if (FormatTok.Tok.is(tok::raw_identifier) && tokenText() == "if") {
      parseIfThenElse();
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

void UnwrappedLineParser::parseLabel() {
  // TODO: remove all asserts!!!!
  assert(FormatTok.Tok.is(tok::colon) && "':' expected");
  nextToken();
  unsigned OldLineLevel = Line.Level;
  if (Line.Level > 0)
    --Line.Level;
  if (FormatTok.Tok.is(tok::l_brace)) {
    parseBlock();
  }
  addUnwrappedLine();
  Line.Level = OldLineLevel;
}

void UnwrappedLineParser::parseCaseLabel() {
  assert(FormatTok.Tok.is(tok::raw_identifier) && tokenText() == "case" &&
         "'case' expected");
  // TODO(alexfh): fix handling of complex expressions here.
  do {
    nextToken();
  } while (!eof() && !FormatTok.Tok.is(tok::colon));
  parseLabel();
}

void UnwrappedLineParser::parseSwitch() {
  assert(FormatTok.Tok.is(tok::raw_identifier) && "Identifier expected");
  nextToken();
  parseParens();
  if (FormatTok.Tok.is(tok::l_brace)) {
    parseBlock();
    addUnwrappedLine();
  } else {
    addUnwrappedLine();
    ++Line.Level;
    parseStatement();
    --Line.Level;
  }
}

void UnwrappedLineParser::parseAccessSpecifier() {
  assert(FormatTok.Tok.is(tok::raw_identifier) && "Identifier expected");
  nextToken();
  nextToken();
  addUnwrappedLine();
}

void UnwrappedLineParser::parseEnum() {
  do {
    nextToken();
    if (FormatTok.Tok.is(tok::semi)) {
      nextToken();
      addUnwrappedLine();
      return;
    }
  } while (!eof());
}

void UnwrappedLineParser::addUnwrappedLine() {
  // Consume trailing comments.
  while (!eof() && FormatTok.NewlinesBefore == 0 &&
         FormatTok.Tok.is(tok::comment)) {
    nextToken();
  }
  Callback.formatUnwrappedLine(Line);
  Line.Tokens.clear();
}

bool UnwrappedLineParser::eof() const {
  return FormatTok.Tok.is(tok::eof);
}

void UnwrappedLineParser::nextToken() {
  if (eof())
    return;
  Line.Tokens.push_back(FormatTok);
  parseToken();
}

void UnwrappedLineParser::parseToken() {
  if (GreaterStashed) {
    FormatTok.NewlinesBefore = 0;
    FormatTok.WhiteSpaceStart = FormatTok.Tok.getLocation().getLocWithOffset(1);
    FormatTok.WhiteSpaceLength = 0;
    GreaterStashed = false;
    return;
  }

  FormatTok = FormatToken();
  Lex.LexFromRawLexer(FormatTok.Tok);
  FormatTok.WhiteSpaceStart = FormatTok.Tok.getLocation();

  // Consume and record whitespace until we find a significant token.
  while (FormatTok.Tok.is(tok::unknown)) {
    FormatTok.NewlinesBefore += tokenText().count('\n');
    FormatTok.WhiteSpaceLength += FormatTok.Tok.getLength();

    if (eof())
      return;
    Lex.LexFromRawLexer(FormatTok.Tok);
  }

  if (FormatTok.Tok.is(tok::greatergreater)) {
    FormatTok.Tok.setKind(tok::greater);
    GreaterStashed = true;
  }
}

StringRef UnwrappedLineParser::tokenText() {
  StringRef Data(SourceMgr.getCharacterData(FormatTok.Tok.getLocation()),
                 FormatTok.Tok.getLength());
  return Data;
}

} // end namespace format
} // end namespace clang
