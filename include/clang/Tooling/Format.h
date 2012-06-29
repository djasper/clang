//===--- Format.h - Format C++ code -----------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  Various functions to configurably format source code.
//
//  This supports two use cases:
//  - Format (ranges in) a given file.
//  - Reformat sources after automated refactoring.
//
//  Formatting is done on a per file basis.
//
//  The formatting strategy is:
//  - lex the file content
//  - in case formatting decisions need information of the AST analysis, run
//    the analysis and annotate all tokens that are relevant for formatting
//    in the current file
//  - reformat based on the (annotated) tokens
//
//  To allow different strategies of handling the SourceManager the formatter
//  works on an interface AnalyzableSource, which provides access to the file
//  contents and, if available, allow to lazily run an analysis over the AST
//  when needed.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_TOOLING_FORMAT_H_
#define LLVM_CLANG_TOOLING_FORMAT_H

#include "clang/Frontend/FrontendAction.h"
#include "clang/Tooling/Refactoring.h"

namespace clang {

class Lexer;
class SourceManager;

namespace tooling {

/// \brief A character range of source code.
struct CodeRange {
  CodeRange(unsigned Offset, unsigned Length)
    : Offset(Offset), Length(Length) {}

  unsigned Offset;
  unsigned Length;
};

/// \brief Reformats the given Ranges in the Source.
Replacements reformat(Lexer &Lex, SourceManager &Sources,
                      std::vector<CodeRange> Ranges);

} // end namespace tooling
} // end namespace clang

#endif // LLVM_CLANG_TOOLING_FORMAT_H
