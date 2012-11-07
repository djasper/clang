//===- unittest/Format/FormatTest.cpp - Formatting unit tests -------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "../Tooling/RewriterTestContext.h"
#include "clang/Lex/Lexer.h"
#include "clang/Format/Format.h"
#include "gtest/gtest.h"

namespace clang {
namespace format {

class FormatTest : public ::testing::Test {
protected:
  std::string format(llvm::StringRef Code, unsigned offset, unsigned length) {
    RewriterTestContext Context;
    FileID ID = Context.createInMemoryFile("input.cc", Code);
    std::vector<CodeRange> Ranges(1, CodeRange(offset, length));
    Lexer Lex(ID, Context.Sources.getBuffer(ID), Context.Sources,
              LangOptions());
    tooling::Replacements Replace = reformat(Lex, Context.Sources, Ranges);
    EXPECT_TRUE(applyAllReplacements(Replace, Context.Rewrite));
    llvm::outs() << Context.getRewrittenText(ID) << "\n";
    return Context.getRewrittenText(ID);
  }
};

TEST_F(FormatTest, DoesNotChangeCorrectlyFormatedCode) {
  EXPECT_EQ(";", format(";", 0, 1));
}

TEST_F(FormatTest, FormatsGlobalStatementsAt0) {
  EXPECT_EQ("int i;", format("  int i;", 0, 1));
  EXPECT_EQ("\nint i;", format(" \n\t \r  int i;", 0, 1));
  EXPECT_EQ("int i;\nint j;", format("    int i; int j;", 0, 1));
  EXPECT_EQ("int i;\nint j;", format("    int i;\n  int j;", 0, 1));
}

TEST_F(FormatTest, FormatsContinuationsAtFirstFormat) {
  EXPECT_EQ("int\n    i;", format("int\ni;", 0, 1));
}

TEST_F(FormatTest, FormatsNestedBlockStatements) {
  EXPECT_EQ("{\n  {\n    {\n    }\n  }\n}", format("{{{}}}", 0, 1));
}

TEST_F(FormatTest, FormatsForLoop) {
  EXPECT_EQ("for (int i = 0; i < 10; ++i);",
            format("for(int i=0;i<10;++i);", 0 , 1));
  EXPECT_EQ("for (int i = 0;\n     i < 10;\n     ++i);",
            format("for(int i=0;\ni<10;\n++i);", 0 , 1));
}

TEST_F(FormatTest, FormatsWhileLoop) {
  EXPECT_EQ("while (true) {\n}", format("while(true){}", 0, 1));
}

TEST_F(FormatTest, FormatsNestedCall) {
  EXPECT_EQ("Method(1,\n"
            "       2(\n"
            "           3));",
            format("Method(1,\n2(\n3));", 0, 1));
  EXPECT_EQ("Method(1(2,\n"
            "         3()));", format("Method(1(2,\n3()));", 0, 1));
}

TEST_F(FormatTest, FormatsAwesomeMethodCall) {
  EXPECT_EQ(
      "SomeLongMethodName(SomeReallyLongMethod(CallOtherReallyLongMethod(\n"
      "    parameter, parameter, parameter)), SecondLongCall(some_parameter));",
      format(
          "SomeLongMethodName(SomeReallyLongMethod(CallOtherReallyLongMethod(\n"
          "parameter , parameter, parameter)), SecondLongCall("
          "some_parameter) );", 0, 1));
  EXPECT_EQ(
      "SomeLongMethodName(SomeReallyLongMethod(CallOtherReallyLongMethod(\n"
      "    parameter, parameter, parameter)), SecondLongCall(some_parameter));",
      format(
          "SomeLongMethodName(SomeReallyLongMethod(CallOtherReallyLongMethod("
          "parameter,parameter,parameter)),SecondLongCall("
          "some_parameter) );", 0, 1));
}

TEST_F(FormatTest, FormatsFunctionDefinition) {
  EXPECT_EQ(
      "void f(int a, int b, int c, int d, int e, int f, int g,"
      " int h, int j, int f,\n       int c, int ddddddddddddd) {\n}",
      format("void f(int a, int b, int c, int d, int e, int f, int g,"
        "int h, int j, int f, int c, int ddddddddddddd) {}", 0, 1));
}

TEST_F(FormatTest, FormatIfWithoutCompountStatement) {
  EXPECT_EQ(
      "if (true)\n  f();\ng();",
      format("if (true) f(); g();", 0, 1));
  EXPECT_EQ(
      "if (a)\n  if (b)\n    if (c)\n      g();\nh();",
      format("if(a)if(b)if(c)g();h();", 0, 1));
  EXPECT_EQ(
      "if (a)\n  if (b) {\n    f();\n  }\ng();",
      format("if(a)if(b) {f();}g();", 0, 1));
}

TEST_F(FormatTest, UnderstandsSingleLineComments) {
  EXPECT_EQ(
      "// line 1\n// line 2\nvoid f() {\n}\n",
      format("// line 1\n// line 2\nvoid f() {}\n", 0, 1));

  EXPECT_EQ(
      "void f() {\n  // Doesn't do anything\n}",
      format("void f() {\n// Doesn't do anything\n}", 0, 1));
}

TEST_F(FormatTest, DoesNotBreakSemiAfterClassDecl) {
  EXPECT_EQ(
      "class A {\n};\n", format("class A{};\n", 0, 1));
}

TEST_F(FormatTest, UnderstandsPPKeywords) {
  EXPECT_EQ(
      "#include <a.h>\\\n", format("#include <a.h>\\\nest\nb\n", 0, 1));
}

} // end namespace tooling
} // end namespace clang
