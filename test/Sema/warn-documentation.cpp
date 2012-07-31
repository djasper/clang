// RUN: %clang_cc1 -fsyntax-only -Wdocumentation -Wdocumentation-pedantic -verify %s

// expected-warning@+1 {{expected quoted string after equals sign}}
/// <a href=>
int test_html1(int);

// expected-warning@+1 {{expected quoted string after equals sign}}
/// <a href==>
int test_html2(int);

// expected-warning@+2 {{expected quoted string after equals sign}}
// expected-warning@+1 {{HTML start tag prematurely ended, expected attribute name or '>'}}
/// <a href= blah
int test_html3(int);

// expected-warning@+1 {{HTML start tag prematurely ended, expected attribute name or '>'}}
/// <a =>
int test_html4(int);

// expected-warning@+1 {{HTML start tag prematurely ended, expected attribute name or '>'}}
/// <a "aaa">
int test_html5(int);

// expected-warning@+1 {{HTML start tag prematurely ended, expected attribute name or '>'}}
/// <a a="b" =>
int test_html6(int);

// expected-warning@+1 {{HTML start tag prematurely ended, expected attribute name or '>'}}
/// <a a="b" "aaa">
int test_html7(int);

// expected-warning@+1 {{HTML start tag prematurely ended, expected attribute name or '>'}}
/// <a a="b" =
int test_html8(int);

// expected-warning@+2 {{HTML start tag prematurely ended, expected attribute name or '>'}} expected-note@+1 {{HTML tag started here}}
/** Aaa bbb<ccc ddd eee
 * fff ggg.
 */
int test_html9(int);

// expected-warning@+1 {{HTML start tag prematurely ended, expected attribute name or '>'}}
/** Aaa bbb<ccc ddd eee 42%
 * fff ggg.
 */
int test_html10(int);

// expected-warning@+1 {{HTML end tag 'br' is forbidden}}
/// <br></br>
int test_html11(int);

/// <blockquote>Meow</blockquote>
int test_html_nesting1(int);

/// <b><i>Meow</i></b>
int test_html_nesting2(int);

/// <p>Aaa<br>
/// Bbb</p>
int test_html_nesting3(int);

/// <p>Aaa<br />
/// Bbb</p>
int test_html_nesting4(int);

// expected-warning@+1 {{HTML end tag does not match any start tag}}
/// <b><i>Meow</a>
int test_html_nesting5(int);

// expected-warning@+2 {{HTML start tag 'i' closed by 'b'}}
// expected-warning@+1 {{HTML end tag does not match any start tag}}
/// <b><i>Meow</b></b>
int test_html_nesting6(int);

// expected-warning@+2 {{HTML start tag 'i' closed by 'b'}}
// expected-warning@+1 {{HTML end tag does not match any start tag}}
/// <b><i>Meow</b></i>
int test_html_nesting7(int);


// expected-warning@+1 {{empty paragraph passed to '\brief' command}}
/// \brief\brief Aaa
int test_block_command1(int);

// expected-warning@+1 {{empty paragraph passed to '\brief' command}}
/// \brief \brief Aaa
int test_block_command2(int);

// expected-warning@+1 {{empty paragraph passed to '\brief' command}}
/// \brief
/// \brief Aaa
int test_block_command3(int);

// expected-warning@+1 {{empty paragraph passed to '\brief' command}}
/// \brief
///
/// \brief Aaa
int test_block_command4(int);

// There is trailing whitespace on one of the following lines, don't remove it!
// expected-warning@+1 {{empty paragraph passed to '\brief' command}}
/// \brief
/// 
/// \brief Aaa
int test_block_command5(int);

/// \brief \c Aaa
int test_block_command6(int);

// expected-warning@+1 {{'\param' command used in a comment that is not attached to a function declaration}}
/// \param a Blah blah.
int test_param1;

// expected-warning@+1 {{empty paragraph passed to '\param' command}}
/// \param
/// \param a Blah blah.
int test_param2(int a);

// expected-warning@+1 {{empty paragraph passed to '\param' command}}
/// \param a
int test_param3(int a);

/// \param a Blah blah.
int test_param4(int a);

/// \param [in] a Blah blah.
int test_param5(int a);

/// \param [out] a Blah blah.
int test_param6(int a);

/// \param [in,out] a Blah blah.
int test_param7(int a);

// expected-warning@+1 {{whitespace is not allowed in parameter passing direction}}
/// \param [ in ] a Blah blah.
int test_param8(int a);

// expected-warning@+1 {{whitespace is not allowed in parameter passing direction}}
/// \param [in, out] a Blah blah.
int test_param9(int a);

// expected-warning@+1 {{unrecognized parameter passing direction, valid directions are '[in]', '[out]' and '[in,out]'}}
/// \param [ junk] a Blah blah.
int test_param10(int a);

// expected-warning@+1 {{parameter 'a' not found in the function declaration}}
/// \param a Blah blah.
int test_param11();

// expected-warning@+1 {{parameter 'A' not found in the function declaration}} expected-note@+1 {{did you mean 'a'?}}
/// \param A Blah blah.
int test_param12(int a);

// expected-warning@+1 {{parameter 'aab' not found in the function declaration}} expected-note@+1 {{did you mean 'aaa'?}}
/// \param aab Blah blah.
int test_param13(int aaa, int bbb);

// expected-warning@+1 {{parameter 'aab' not found in the function declaration}}
/// \param aab Blah blah.
int test_param14(int bbb, int ccc);

class C {
  // expected-warning@+1 {{parameter 'aaa' not found in the function declaration}}
  /// \param aaa Blah blah.
  C(int bbb, int ccc);

  // expected-warning@+1 {{parameter 'aaa' not found in the function declaration}}
  /// \param aaa Blah blah.
 int test_param15(int bbb, int ccc);
};

// expected-warning@+1 {{parameter 'aab' not found in the function declaration}}
/// \param aab Blah blah.
template<typename T>
void test_param16(int bbb, int ccc);

// expected-warning@+3 {{parameter 'a' is already documented}}
// expected-note@+1 {{previous documentation}}
/// \param a Aaa.
/// \param a Aaa.
int test_param17(int a);

// expected-warning@+4 {{parameter 'x2' is already documented}}
// expected-note@+2 {{previous documentation}}
/// \param x1 Aaa.
/// \param x2 Bbb.
/// \param x2 Ccc.
int test_param18(int x1, int x2, int x3);


// expected-warning@+1 {{'\tparam' command used in a comment that is not attached to a template declaration}}
/// \tparam T Aaa
int test_tparam1;

// expected-warning@+1 {{'\tparam' command used in a comment that is not attached to a template declaration}}
/// \tparam T Aaa
void test_tparam2(int aaa);

// expected-warning@+1 {{empty paragraph passed to '\tparam' command}}
/// \tparam
/// \param aaa Blah blah
template<typename T>
void test_tparam3(T aaa);

// expected-warning@+1 {{template parameter 'T' not found in the template declaration}} expected-note@+1 {{did you mean 'TT'?}}
/// \tparam T Aaa
template<typename TT>
void test_tparam4(TT aaa);

// expected-warning@+1 {{template parameter 'T' not found in the template declaration}} expected-note@+1 {{did you mean 'TT'?}}
/// \tparam T Aaa
template<typename TT>
class test_tparam5 {
  // expected-warning@+1 {{template parameter 'T' not found in the template declaration}} expected-note@+1 {{did you mean 'TTT'?}}
  /// \tparam T Aaa
  template<typename TTT>
  void test_tparam6(TTT aaa);
};

/// \tparam T1 Aaa
/// \tparam T2 Bbb
template<typename T1, typename T2>
void test_tparam7(T1 aaa, T2 bbb);

// expected-warning@+1 {{template parameter 'SomTy' not found in the template declaration}} expected-note@+1 {{did you mean 'SomeTy'?}}
/// \tparam SomTy Aaa
/// \tparam OtherTy Bbb
template<typename SomeTy, typename OtherTy>
void test_tparam8(SomeTy aaa, OtherTy bbb);

// expected-warning@+2 {{template parameter 'T1' is already documented}} expected-note@+1 {{previous documentation}}
/// \tparam T1 Aaa
/// \tparam T1 Bbb
template<typename T1, typename T2>
void test_tparam9(T1 aaa, T2 bbb);

/// \tparam T Aaa
/// \tparam TT Bbb
template<template<typename T> class TT>
void test_tparam10(TT<int> aaa);

/// \tparam T Aaa
/// \tparam TT Bbb
/// \tparam TTT Ccc
template<template<template<typename T> class TT, class C> class TTT>
void test_tparam11();

/// \tparam I Aaa
template<int I>
void test_tparam12();


// expected-warning@+1 {{empty paragraph passed to '\brief' command}}
int test1; ///< \brief\brief Aaa

// expected-warning@+2 {{empty paragraph passed to '\brief' command}}
// expected-warning@+2 {{empty paragraph passed to '\brief' command}}
int test2, ///< \brief\brief Aaa
    test3; ///< \brief\brief Aaa

// expected-warning@+1 {{empty paragraph passed to '\brief' command}}
int test4; ///< \brief
           ///< \brief Aaa


// Check that we attach the comment to the declaration during parsing in the
// following cases.  The test is based on the fact that we don't parse
// documentation comments that are not attached to anything.

// expected-warning@+1 {{empty paragraph passed to '\brief' command}}
/// \brief\brief Aaa
int test_attach1;

// expected-warning@+1 {{empty paragraph passed to '\brief' command}}
/// \brief\brief Aaa
int test_attach2(int);

// expected-warning@+1 {{empty paragraph passed to '\brief' command}}
/// \brief\brief Aaa
struct test_attach3 {
  // expected-warning@+1 {{empty paragraph passed to '\brief' command}}
  /// \brief\brief Aaa
  int test_attach4;

  // expected-warning@+1 {{empty paragraph passed to '\brief' command}}
  int test_attach5; ///< \brief\brief Aaa

  // expected-warning@+1 {{empty paragraph passed to '\brief' command}}
  /// \brief\brief Aaa
  int test_attach6(int);
};

// expected-warning@+1 {{empty paragraph passed to '\brief' command}}
/// \brief\brief Aaa
class test_attach7 {
  // expected-warning@+1 {{empty paragraph passed to '\brief' command}}
  /// \brief\brief Aaa
  int test_attach8;

  // expected-warning@+1 {{empty paragraph passed to '\brief' command}}
  int test_attach9; ///< \brief\brief Aaa

  // expected-warning@+1 {{empty paragraph passed to '\brief' command}}
  /// \brief\brief Aaa
  int test_attach10(int);
};

// expected-warning@+1 {{empty paragraph passed to '\brief' command}}
/// \brief\brief Aaa
enum test_attach9 {
  // expected-warning@+1 {{empty paragraph passed to '\brief' command}}
  /// \brief\brief Aaa
  test_attach10,

  // expected-warning@+1 {{empty paragraph passed to '\brief' command}}
  test_attach11 ///< \brief\brief Aaa
};

// expected-warning@+1 {{empty paragraph passed to '\brief' command}}
/// \brief\brief Aaa
struct test_noattach12 *test_attach13;

// expected-warning@+1 {{empty paragraph passed to '\brief' command}}
/// \brief\brief Aaa
typedef struct test_noattach14 *test_attach15;

// expected-warning@+1 {{empty paragraph passed to '\brief' command}}
/// \brief\brief Aaa
typedef struct test_attach16 { int a; } test_attach17;

struct S { int a; };

// expected-warning@+1 {{empty paragraph passed to '\brief' command}}
/// \brief\brief Aaa
struct S *test_attach18;

// expected-warning@+1 {{empty paragraph passed to '\brief' command}}
/// \brief\brief Aaa
typedef struct S *test_attach19;

// expected-warning@+1 {{empty paragraph passed to '\brief' command}}
/// \brief\brief Aaa
struct test_attach20;

// expected-warning@+1 {{empty paragraph passed to '\brief' command}}
/// \brief\brief Aaa
typedef struct test_attach21 {
  // expected-warning@+1 {{empty paragraph passed to '\brief' command}}
  /// \brief\brief Aaa
  int test_attach22;
} test_attach23;

// expected-warning@+1 {{empty paragraph passed to '\brief' command}}
/// \brief\brief Aaa
namespace test_attach24 {
  // expected-warning@+1 {{empty paragraph passed to '\brief' command}}
  /// \brief\brief Aaa
  namespace test_attach25 {
  }
}

// expected-warning@+1 {{empty paragraph passed to '\brief' command}}
/// \brief\brief Aaa
/// \tparam T Aaa
template<typename T>
void test_attach26(T aaa);

// expected-warning@+1 {{empty paragraph passed to '\brief' command}}
/// \brief\brief Aaa
/// \tparam T Aaa
template<typename T, typename U>
void test_attach27(T aaa, U bbb);

// expected-warning@+2 {{empty paragraph passed to '\brief' command}}
// expected-warning@+2 {{template parameter 'T' not found in the template declaration}}
/// \brief\brief Aaa
/// \tparam T Aaa
template<>
void test_attach27(int aaa, int bbb);

// expected-warning@+1 {{empty paragraph passed to '\brief' command}}
/// \brief\brief Aaa
/// \tparam T Aaa
template<typename T>
class test_attach28 {
  T aaa;
};

// expected-warning@+1 {{empty paragraph passed to '\brief' command}}
/// \brief\brief Aaa
/// \tparam T Aaa
template<typename T, typename U>
class test_attach29 { };

// expected-warning@+1 {{empty paragraph passed to '\brief' command}}
/// \brief\brief Aaa
/// \tparam T Aaa
template<typename T>
class test_attach29<T, int> { };

// expected-warning@+1 {{empty paragraph passed to '\brief' command}}
/// \brief\brief Aaa
template<>
class test_attach29<int, int> { };

// expected-warning@+1 {{empty paragraph passed to '\brief' command}}
/// \brief\brief Aaa
/// \tparam T Aaa
template<typename T, typename U, typename V>
class test_attach30 { };

// expected-warning@+1 {{empty paragraph passed to '\brief' command}}
/// \brief\brief Aaa
/// \tparam T Aaa
template<typename T, typename U>
class test_attach30<T, U, int> { };

// expected-warning@+1 {{empty paragraph passed to '\brief' command}}
/// \brief\brief Aaa
/// \tparam T Aaa
template<typename T>
class test_attach30<T, int, int> { };

// expected-warning@+2 {{empty paragraph passed to '\brief' command}}
// expected-warning@+2 {{template parameter 'T' not found in the template declaration}}
/// \brief\brief Aaa
/// \tparam T Aaa
template<>
class test_attach30<int, int, int> { };

// expected-warning@+1 {{empty paragraph passed to '\brief' command}}
/// \brief\brief Aaa
class test_attach31 {
  // expected-warning@+1 {{empty paragraph passed to '\brief' command}}
  /// \brief\brief Aaa
  /// \tparam T Aaa
  template<typename T, typename U>
  void test_attach32(T aaa, U bbb);
};

template<typename T>
class test_attach33 {
  // expected-warning@+2 {{empty paragraph passed to '\brief' command}}
  // expected-warning@+2 {{template parameter 'T' not found in the template declaration}}
  /// \brief\brief Aaa
  /// \tparam T Aaa
  template<typename TT, typename UU>
  void test_attach34(TT aaa, UU bbb);
};

// expected-warning@+2 {{empty paragraph passed to '\brief' command}}
// expected-warning@+2 {{template parameter 'T' not found in the template declaration}}
/// \brief\brief Aaa
/// \tparam T Aaa
template<> template<>
void test_attach33<int>::test_attach34(int aaa, int bbb) {}

template<typename T>
class test_attach35 {
  // expected-warning@+2 {{empty paragraph passed to '\brief' command}}
  // expected-warning@+2 {{'\tparam' command used in a comment that is not attached to a template declaration}}
  /// \brief\brief Aaa
  /// \tparam T Aaa
  void test_attach36(int aaa, int bbb);
};

// expected-warning@+1 {{empty paragraph passed to '\brief' command}}
/// \brief\brief Aaa
/// \tparam T Aaa
template<typename T>
void test_attach35<T>::test_attach36(int aaa, int bbb) {}

// expected-warning@+2 {{empty paragraph passed to '\brief' command}}
// expected-warning@+2 {{template parameter 'T' not found in the template declaration}}
/// \brief\brief Aaa
/// \tparam T Aaa
template<>
void test_attach35<int>::test_attach36(int aaa, int bbb) {}


// PR13411, reduced.  We used to crash on this.
/**
 * @code Aaa.
 */
void test_nocrash1(int);

// We used to crash on this.
// expected-warning@+2 {{empty paragraph passed to '\param' command}}
// expected-warning@+1 {{empty paragraph passed to '\brief' command}}
/// \param\brief
void test_nocrash2(int);

