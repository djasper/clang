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

#ifndef LLVM_CLANG_FORMAT_CONTINUATION_PARSER_H
#define LLVM_CLANG_FORMAT_CONTINUATION_PARSER_H

#include "clang/Basic/SourceManager.h"
#include "clang/Lex/Lexer.h"

namespace clang {
namespace format {

struct FormatToken {
  FormatToken() : NewlinesBefore(0), WhiteSpaceLength(0) {}

  Token Tok;
  unsigned NewlinesBefore;
  unsigned WhiteSpaceLength;
  SourceLocation WhiteSpaceStart;
};

struct Continuation {
  Continuation() : Level(0) {}

  std::vector<FormatToken> Tokens;
  unsigned Level;
};

class ContinuationConsumer {
public:
  virtual void formatContinuation(const Continuation &Cont) = 0;
};

class ContinuationParser {
public:
  ContinuationParser(Lexer &Lex, SourceManager &Sources,
                     ContinuationConsumer &Callback);

  void parse();

private:
  void parseLevel();
  void parseBlock();
  void parsePPDirective();
  void parseComment();
  void parseStatement();
  void parseParens();
  void parseIfThenElse();
  void addContinuation();
  bool eof() const;
  const FormatToken &current() const;
  FormatToken &current();
  bool nextToken();
  bool parseToken();

  Continuation Cont;
  //std::vector<FormatToken> Seq;
  //unsigned StartLevel;
  FormatToken FormatTok;
  //FormatToken FormatTok;
  unsigned Level;
  bool Eof;

  Lexer &Lex;
  SourceManager &Sources;
  ContinuationConsumer &Callback;
};

} // end namespace format
} // end namespace clang

#endif // LLVM_CLANG_FORMAT_CONTINUATION_PARSER_H
