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
    LangOptions LangOpts;
    LangOpts.CPlusPlus = 1;
    Lexer Lex(ID, Context.Sources.getBuffer(ID), Context.Sources, LangOpts);
    tooling::Replacements Replace =
        reformat(getLLVMStyle(), Lex, Context.Sources, Ranges);
    EXPECT_TRUE(applyAllReplacements(Replace, Context.Rewrite));
    //llvm::outs() << Context.getRewrittenText(ID) << "\n";
    return Context.getRewrittenText(ID);
  }

  std::string format(llvm::StringRef Code) {
    return format(Code, 0, Code.size());
  }

  void verifyFormat(llvm::StringRef Code) {
    std::string WithoutFormat(Code.str());
    for (unsigned i = 0, e = WithoutFormat.size(); i != e; ++i) {
      if (WithoutFormat[i] == '\n')
        WithoutFormat[i] = ' ';
    }
    EXPECT_EQ(Code.str(), format(WithoutFormat));
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
  EXPECT_EQ("int i;", format("int\ni;"));
}

TEST_F(FormatTest, FormatsNestedBlockStatements) {
  EXPECT_EQ("{\n  {\n    {\n    }\n  }\n}", format("{{{}}}"));
}

TEST_F(FormatTest, FormatsForLoop) {
  verifyFormat(
      "for (int VeryVeryLongLoopVariable = 0; VeryVeryLongLoopVariable < 10;\n"
      "     ++VeryVeryLongLoopVariable);");
}

TEST_F(FormatTest, FormatsWhileLoop) {
  verifyFormat("while (true) {\n}");
}

TEST_F(FormatTest, FormatsNestedCall) {
  verifyFormat("Method(f1, f2(f3));");
  verifyFormat("Method(f1(f2, f3()));");
}

TEST_F(FormatTest, FormatsAwesomeMethodCall) {
  verifyFormat(
      "SomeLongMethodName(SomeReallyLongMethod(CallOtherReallyLongMethod(\n"
      "    parameter, parameter, parameter)), SecondLongCall(some_parameter));");
}

TEST_F(FormatTest, FormatsFunctionDefinition) {
  verifyFormat(
      "void f(int a, int b, int c, int d, int e, int f, int g,"
          " int h, int j, int f,\n"
      "       int c, int ddddddddddddd) {\n"
      "}");
}

TEST_F(FormatTest, FormatIfWithoutCompountStatement) {
  verifyFormat("if (true)\n  f();\ng();");
  verifyFormat("if (a)\n  if (b)\n    if (c)\n      g();\nh();");
  verifyFormat("if (a)\n  if (b) {\n    f();\n  }\ng();");
}

TEST_F(FormatTest, ParseIfThenElse) {
  verifyFormat(
      "if (true)\n"
      "  if (true)\n"
      "    if (true)\n"
      "      f();\n"
      "    else\n"
      "      g();\n"
      "  else\n"
      "    h();\n"
      "else\n"
      "  i();");
  verifyFormat(
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
      "}");
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
  verifyFormat("class A {\n};");
}

TEST_F(FormatTest, BreaksAsHighAsPossible) {
  verifyFormat(
      "if ((aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa && aaaaaaaaaaaaaaaaaaaaaaaaaa) ||\n"
      "    (bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb && bbbbbbbbbbbbbbbbbbbbbbbbbb))\n"
      "  f();");
}

TEST_F(FormatTest, ElseIf) {
  verifyFormat("if (a) {\n"
               "} else if (b) {\n"
               "}");
  verifyFormat("if (a)\n"
               "  f();\n"
               "else if (b)\n"
               "  g();\n"
               "else\n"
               "  h();");
}

TEST_F(FormatTest, UnderstandsAccessSpecifiers) {
  verifyFormat("class A {\n"
               "public:\n"
               "protected:\n"
               "private:\n"
               "  void f() {\n"
               "  }\n"
               "};");
}

TEST_F(FormatTest, SwitchStatement) {
  verifyFormat("switch(x) {\n"
               "case 1:\n"
               "  f();\n"
               "  break;\n"
               "case kFoo:\n"
               "case ns::kBar:\n"
               "case kBaz:\n"
               "  break;\n"
               "default:\n"
               "  g();\n"
               "  break;\n"
               "}");
  verifyFormat("switch(x) {\n"
               "case 1: {\n"
               "  f();\n"
               "  break;\n"
               "}\n"
               "}");
  verifyFormat("switch(test)\n"
               "  ;");
}
} // end namespace tooling
} // end namespace clang
