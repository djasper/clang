//===--- Format.cpp - Format C++ code -------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  Implements Format.
//
//===----------------------------------------------------------------------===//

#include "clang/Tooling/Format.h"

#include "clang/Basic/SourceManager.h"
#include "clang/Lex/Lexer.h"

namespace clang {
namespace tooling {

struct MyToken {
  SourceLocation Begin;
  SourceLocation End;
  StringRef Data;
  unsigned Length;
  bool BreakBefore;
  SourceLocation WSBefore;
  unsigned WSLength;
};
  

class Formatter {
 public:
  Formatter() {}
/*
  int replaceWS(unsigned Pos, StringRef Rep) {
    SourceLocation startLoc = Tokens[Pos].getLocation();
    unsigned len = 0;
    while (Pos < Tokens.size() && Tokens[Pos].getKind() == tok::unknown) {
      len += Tokens[Pos].getLength();
      ++Pos;
    }
    llvm::outs() << Pos << " " << len << "\n";
    if (Pos == Tokens.size())
      return Pos;
    Replaces.insert(Replacement(*Sources, startLoc, len, Rep));
    return Pos;
  }

  int reformatEntity(unsigned Pos, unsigned Indent) {
    std::vector<int> openingParens;
    int column = 0;
    bool requiresWS = false;
    bool prevTokenIsIdentifier = false;
    SourceLocation wsStart;
    unsigned wsLen = 0;
    while (Pos < Tokens.size()) {
      switch (Tokens[Pos].getKind()) {
      case tok::unknown:
        if (wsLen == 0)
          wsStart = Tokens[Pos].getLocation();
        wsLen += Tokens[Pos].getLength();
        ++Pos;
        continue;
      case tok::semi:
      case tok::r_brace:
      case tok::l_brace:
        return replaceWS(Pos + 1, "\n");
      case tok::l_paren:
        openingParens.push_back(column);
        break;
      case tok::r_paren:
        openingParens.pop_back();
        break;
      case tok::raw_identifier:
        if (prevTokenIsIdentifier)
          requiresWS = true;
        prevTokenIsIdentifier = true;
      default:
        break;
      }
      column += Tokens[Pos].getLength() + (requiresWS ? 1 : 0);
      if (wsLen > 0)
        Replaces.insert(Replacement(*Sources, wsStart, wsLen, (requiresWS ? " " : "")));
      wsLen = 0;
      ++Pos;
    }
    return Pos;
  }

*/
    
  unsigned reindent(unsigned Pos, unsigned Indent) {
    StringRef lb = "\n                                                      ";
    if (Tokens[Pos].WSLength > 0 || Indent > 0)
      Replaces.insert(Replacement(*Sources, Tokens[Pos].WSBefore, Tokens[Pos].WSLength, lb.substr(1, Indent)));
    std::vector<unsigned> IndentLevel;
    IndentLevel.push_back(Indent);
    while (Pos < Tokens.size()) {
      if (Tokens[Pos].BreakBefore) {
        Replaces.insert(Replacement(*Sources, Tokens[Pos].WSBefore, Tokens[Pos].WSLength, lb.substr(Indent + 1))); 
      }
      ++Pos;
    }
    return Pos;
  }

  void reformat(Lexer &Lex, SourceManager *S,
                std::vector<CodeRange> Ranges) {
    Sources = S;
    Lex.SetKeepWhitespaceMode(true);
    bool sawNewline = false;
    SourceLocation wsStart = SourceLocation();
    unsigned wsLength = 0;
    while (true) {
      Token t;
      bool end = Lex.LexFromRawLexer(t);
      if (!wsStart.isValid())
        wsStart = t.getLocation();
      MyToken token;
      token.Begin = t.getLocation();
      token.Length = t.getLength();
      token.End = Lex.getLocForEndOfToken(token.Begin, /*Offset=*/0,
          *Sources, LangOptions());
      token.Data = Sources->getCharacterData(t.getLocation()), t.getLength();
      if (t.getKind() == tok::unknown) {
        if (token.Data[0] == '\n')
          sawNewline = true;
        if (wsLength == 0)
          wsStart = t.getLocation();
        wsLength += t.getLength();
      } else {
        token.BreakBefore = sawNewline;
        token.WSBefore = wsStart;
        token.WSLength = wsLength;
        wsLength = 0;
        wsStart = token.End;
        sawNewline = false;
        Tokens.push_back(token);
      }
      if (end)
        break;
    }
    unsigned Pos = 0; //replaceWS(0, "");
    while (Pos < Tokens.size())
      Pos = reindent(Pos, 0);
    //  Pos = reformatEntity(Pos, 0);
  }

  Replacements GetReplacements() {
    return Replaces;
  }

 private:
  SourceManager *Sources;
  std::vector<MyToken> Tokens;
  std::vector<int> newlineDesiredAfter;
  Replacements Replaces;
};

Replacements reformat(Lexer &Lex, SourceManager &Sources,
                      std::vector<CodeRange> Ranges) {
  Formatter f;
  f.reformat(Lex, &Sources, Ranges);
/*  bool First = true;
  bool Continuation = false;
  for (int i = 0; i < tokens.size(); i++) {
    Token tok = tokens[i];
    SourceLocation loc = tok.getLocation();
    StringRef Data(Sources.getCharacterData(loc), tok.getLength());
    llvm::outs() << tok.getKind() << " " << tok.getName() << " " << tok.getLength() << "\n";
    llvm::outs() << "\"" << Data << "\"\n";
    if (tok.getKind() == tok::semi) {
      Continuation = false;
    } else if (tok.getKind() != tok::unknown) {
      Continuation = true;
    }
    if (Data[0] == '\n' || (First && tok.getKind() == tok::unknown)) {
      std::string rep = First ?  "" : "\n";
      if (Continuation) rep += "  ";
      Replacement Replace(Sources, loc, tok.getLength(), rep);
      Replaces.insert(Replace);
    }
    First = false;
  }*/
  return f.GetReplacements();
}

}  // namespace tooling
}  // namespace clang
