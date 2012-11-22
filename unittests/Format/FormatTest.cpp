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
    //llvm::outs() << Context.getRewrittenText(ID) << "\n";
    return Context.getRewrittenText(ID);
  }

  std::string format(llvm::StringRef Code) {
    return format(Code, 0, Code.size());
  }
};

TEST_F(FormatTest, DoesNotChangeCorrectlyFormatedCode) {
  EXPECT_EQ(";", format(";"));
}

TEST_F(FormatTest, FormatsGlobalStatementsAt0) {
  EXPECT_EQ("int i;", format("  int i;"));
  EXPECT_EQ("\nint i;", format(" \n\t \r  int i;"));
  EXPECT_EQ("int i;\nint j;", format("    int i; int j;"));
  EXPECT_EQ("int i;\nint j;", format("    int i;\n  int j;"));
}

TEST_F(FormatTest, FormatsUnwrappedLinesAtFirstFormat) {
  EXPECT_EQ("int\n    i;", format("int\ni;"));
}

TEST_F(FormatTest, FormatsNestedBlockStatements) {
  EXPECT_EQ("{\n  {\n    {\n    }\n  }\n}", format("{{{}}}"));
}

TEST_F(FormatTest, FormatsForLoop) {
  EXPECT_EQ("for (int i = 0; i < 10; ++i);",
            format("for(int i=0;i<10;++i);"));
  EXPECT_EQ("for (int i = 0;\n     i < 10;\n     ++i);",
            format("for(int i=0;\ni<10;\n++i);"));
}

TEST_F(FormatTest, FormatsWhileLoop) {
  EXPECT_EQ("while (true) {\n}", format("while(true){}"));
}

TEST_F(FormatTest, FormatsNestedCall) {
  EXPECT_EQ("Method(1,\n"
            "       2(\n"
            "           3));",
            format("Method(1,\n2(\n3));"));
  EXPECT_EQ("Method(1(2,\n"
            "         3()));", format("Method(1(2,\n3()));"));
}

TEST_F(FormatTest, FormatsAwesomeMethodCall) {
  EXPECT_EQ(
      "SomeLongMethodName(SomeReallyLongMethod(CallOtherReallyLongMethod(\n"
      "    parameter, parameter, parameter)), SecondLongCall(some_parameter));",
      format(
          "SomeLongMethodName(SomeReallyLongMethod(CallOtherReallyLongMethod(\n"
          "parameter , parameter, parameter)), SecondLongCall("
          "some_parameter) );"));
  EXPECT_EQ(
      "SomeLongMethodName(SomeReallyLongMethod(CallOtherReallyLongMethod(\n"
      "    parameter, parameter, parameter)), SecondLongCall(some_parameter));",
      format(
          "SomeLongMethodName(SomeReallyLongMethod(CallOtherReallyLongMethod("
          "parameter,parameter,parameter)),SecondLongCall("
          "some_parameter) );"));
}

TEST_F(FormatTest, FormatsFunctionDefinition) {
  EXPECT_EQ(
      "void f(int a, int b, int c, int d, int e, int f, int g,"
      " int h, int j, int f,\n       int c, int ddddddddddddd) {\n}",
      format("void f(int a, int b, int c, int d, int e, int f, int g,"
        "int h, int j, int f, int c, int ddddddddddddd) {}"));
}

TEST_F(FormatTest, FormatIfWithoutCompountStatement) {
  EXPECT_EQ(
      "if (true)\n  f();\ng();",
      format("if (true) f(); g();"));
  EXPECT_EQ(
      "if (a)\n  if (b)\n    if (c)\n      g();\nh();",
      format("if(a)if(b)if(c)g();h();"));
  EXPECT_EQ(
      "if (a)\n  if (b) {\n    f();\n  }\ng();",
      format("if(a)if(b) {f();}g();"));
}

TEST_F(FormatTest, ParseIfThenElse) {
  EXPECT_EQ(
      "if (true)\n"
      "  if (true)\n"
      "    if (true)\n"
      "      f();\n"
      "    else\n"
      "      g();\n"
      "  else\n"
      "    h();\n"
      "else\n"
      "  i();",
      format("if(true)\nif(true)\nif(true)\nf();\n"
             "else\ng();\nelse\nh();\nelse\ni();"));
  EXPECT_EQ(
      "if (true)\n"
      "  if (true)\n"
      "    if (true) {\n"
      "      if (true)\n"
      "        f();\n"
      "    } else {\n"
      "      g();\n"
      "    }\n"
      "  else\n"
      "    h();\n"
      "else {\n"
      "  i();\n"
      "}",
      format("if(true)\nif(true)\nif(true){\nif(true)f();\n"
             "}else{\ng();\n}\nelse\nh();\nelse{\ni();\n}"));
}

TEST_F(FormatTest, UnderstandsSingleLineComments) {
  EXPECT_EQ(
      "// line 1\n// line 2\nvoid f() {\n}\n",
      format("// line 1\n// line 2\nvoid f() {}\n"));

  EXPECT_EQ(
      "void f() {\n  // Doesn't do anything\n}",
      format("void f() {\n// Doesn't do anything\n}"));
}

TEST_F(FormatTest, DoesNotBreakSemiAfterClassDecl) {
  EXPECT_EQ(
      "class A {\n};\n", format("class A{};\n"));
}

TEST_F(FormatTest, UnderstandsPPKeywords) {
  EXPECT_EQ(
      "#include <a.h>\\\nest\nb\n", format("#include <a.h>\\\nest\nb\n"));
}

} // end namespace tooling
} // end namespace clang
