// RUN: %clang_cc1 -fsyntax-only -verify -ftemplate-depth 512 -ftemplate-backtrace-limit 4 %s

// XFAIL: mingw32

template<int N> struct S {
  typedef typename S<N-1>::type type;
  static int f(int n = S<N-1>::f()); // \
// expected-error{{recursive template instantiation exceeded maximum depth of 512}} \
// expected-note 3 {{instantiation of default function argument}} \
// expected-note {{skipping 509 contexts in backtrace}} \
// expected-note {{use -ftemplate-depth=N to increase recursive template instantiation depth}}

};
template<> struct S<0> {
  typedef int type;
};

// Incrementally instantiate up to S<2048>.
template struct S<256>;
template struct S<512>;
template struct S<768>;
template struct S<1024>;
template struct S<1280>;
template struct S<1536>;
template struct S<1792>;
template struct S<2048>;

// Check that we actually bail out when we hit the instantiation depth limit for
// the default arguments.
void g() { S<2048>::f(); } // expected-note {{required here}}
