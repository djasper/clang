//===--- Format.h - Format C++ code -----------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Various functions to configurably format source code.
///
/// This is EXPERIMENTAL code under heavy development. It is not in a state yet,
/// where it can be used to format real code.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_FORMAT_FORMAT_H_
#define LLVM_CLANG_FORMAT_FORMAT_H

#include "clang/Frontend/FrontendAction.h"
#include "clang/Tooling/Refactoring.h"

namespace clang {

class Lexer;
class SourceManager;

namespace format {

/// \brief A character range of source code.
struct CodeRange {
  CodeRange(unsigned Offset, unsigned Length)
    : Offset(Offset), Length(Length) {}

  unsigned Offset;
  unsigned Length;
};

struct FormatStyle {
  /// \brief The column limit.
  unsigned ColumnLimit;

  /// \brief The maximum number of consecutive empty lines to keep.
  unsigned MaxEmptyLinesToKeep;

  /// \brief Set whether & and * bind to the type as opposed to the variable.
  bool PointerAndReferenceBindToType;

  /// \brief The extra indent or outdent of access modifiers (e.g.: public:).
  int AccessModifierOffset;

  bool SplitTemplateClosingGreater;
};

FormatStyle getLLVMStyle();

FormatStyle getGoogleStyle();

/// \brief Reformats the given Ranges in the token stream coming out of \c Lex.
///
/// Ranges are extended to include full unwrapped lines.
/// TODO(alexfh): Document what unwrapped lines are.
tooling::Replacements reformat(const FormatStyle &Style, Lexer &Lex,
                               SourceManager &SourceMgr,
                               std::vector<CodeRange> Ranges);

} // end namespace format
} // end namespace clang

#endif // LLVM_CLANG_FORMAT_FORMAT_H
