// RUN: %clang_cc1 -analyze -analyzer-checker=core,experimental.core,debug.ExprInspection -analyzer-store=region -analyzer-constraints=range -verify -Wno-null-dereference %s

void clang_analyzer_eval(bool);

typedef typeof(sizeof(int)) size_t;
void malloc (size_t);

void f1() {
  int const &i = 3;
  int b = i;

  int *p = 0;

  if (b != 3)
    *p = 1; // no-warning
}

char* ptr();
char& ref();

// These next two tests just shouldn't crash.
char t1 () {
  ref() = 'c';
  return '0';
}

// just a sanity test, the same behavior as t1()
char t2 () {
  *ptr() = 'c';
  return '0';
}

// Each of the tests below is repeated with pointers as well as references.
// This is mostly a sanity check, but then again, both should work!
char t3 () {
  char& r = ref();
  r = 'c'; // no-warning
  if (r) return r;
  return *(char*)0; // no-warning
}

char t4 () {
  char* p = ptr();
  *p = 'c'; // no-warning
  if (*p) return *p;
  return *(char*)0; // no-warning
}

char t5 (char& r) {
  r = 'c'; // no-warning
  if (r) return r;
  return *(char*)0; // no-warning
}

char t6 (char* p) {
  *p = 'c'; // no-warning
  if (*p) return *p;
  return *(char*)0; // no-warning
}


// PR13440 / <rdar://problem/11977113>
// Test that the array-to-pointer decay works for array references as well.
// More generally, when we want an lvalue for a reference field, we still need
// to do one level of load.
namespace PR13440 {
  typedef int T[1];
  struct S {
    T &x;

    int *m() { return x; }
  };

  struct S2 {
    int (&x)[1];

    int *m() { return x; }
  };

  void test() {
    int a[1];
    S s = { a };
    S2 s2 = { a };

    if (s.x != a) return;
    if (s2.x != a) return;

    a[0] = 42;
    clang_analyzer_eval(s.x[0] == 42); // expected-warning{{TRUE}}
    clang_analyzer_eval(s2.x[0] == 42); // expected-warning{{TRUE}}
  }
}
