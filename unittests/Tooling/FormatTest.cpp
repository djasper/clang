//===- unittest/Tooling/FormatTest.cpp - Formatting unit tests ------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "RewriterTestContext.h"
#include "clang/Lex/Lexer.h"
#include "clang/Tooling/Format.h"
#include "gtest/gtest.h"

namespace clang {
namespace tooling {

class  FormatTest : public ::testing::Test {
protected:
  std::string Format(llvm::StringRef Code, unsigned offset, unsigned length) {
    RewriterTestContext Context;
    FileID ID = Context.createInMemoryFile("input.cc", Code);
    std::vector<CodeRange> Ranges(1, CodeRange(offset, length));
    Lexer Lex(ID, Context.Sources.getBuffer(ID), Context.Sources, LangOptions());
    Replacements Replace = reformat(Lex, Context.Sources, Ranges);
    EXPECT_TRUE(applyAllReplacements(Replace, Context.Rewrite));
    return Context.getRewrittenText(ID);
  }
};

TEST_F(FormatTest, DoesNotChangeCorrectlyFormatedCode) {
  EXPECT_EQ(";", Format(";", 0, 1));
}

TEST_F(FormatTest, FormatsGlobalStatementsAt0) {
//  EXPECT_EQ("int i;", Format("  int i;", 0, 1));
//  EXPECT_EQ("int i;", Format(" \n\t \r  int i;", 0, 1));
//  EXPECT_EQ("int i;\nint j;", Format("    int i; int j;", 0, 1));
//  EXPECT_EQ("int i;\nint j;", Format("    int i;\n  int j;", 0, 1));
}

TEST_F(FormatTest, FormatsContinuationsAtFirstFormat) {
  EXPECT_EQ("int i;", Format("int\ni;", 0, 1));
}

TEST_F(FormatTest, BlockComment) {
  EXPECT_EQ("/* Test test test\n A, B, C */",
      Format("/* Test test test\n A, B, C */", 0, 1));
}

TEST_F(FormatTest, SingleLineComments) {
  EXPECT_EQ("// a\n// b\n", Format("// a\n// b\n", 0, 1));
}

TEST_F(FormatTest, MultiLineString) {
  EXPECT_EQ("\"a\"\n\"b\"\n", Format("\"a\"\n\"b\"\n", 0, 1));
}

TEST_F(FormatTest, Includes) {
  EXPECT_EQ("#include <a.h>\n", Format("#include <a.h>", 0, 1));
}

TEST_F(FormatTest, Templates) {
  EXPECT_EQ("template<typename T> struct A {};",
      Format("template<typename T> struct A {};", 0, 1));
}

TEST_F(FormatTest, NestedTemplate) {
  EXPECT_EQ("vector<vector<int> > a;", Format("vector<vector<int> > a;", 0, 1));
  EXPECT_EQ("vector<vector<int>> a;", Format("vector<vector<int>> a;", 0, 1));
}

TEST_F(FormatTest, Expr) {
  EXPECT_EQ("a << b", Format("a << b", 0, 1));
}

} // end namespace tooling
} // end namespace clang
