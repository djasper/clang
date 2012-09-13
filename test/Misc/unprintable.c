// RUN: %clang_cc1 %s -fmessage-length 40 2>&1 | FileCheck -strict-whitespace %s

int main() {
    int i;
    if((i==/*👿*/1));

// CHECK: {{^    if\(\(i==/\*<U\+1F47F>\*/1\)\);}}

// CHECK: {{^        ~\^~~~~~~~~~~~~~~~}}
// CHECK: {{^       ~ \^               ~}}

    (void)"�ѿ�";

// CHECK: {{^    \(void\)"<CA><U\+047F><F4>";}}
// CHECK: {{^           \^~~~}}

  int n = 0;

// CHECK: {{^<U\+00A0> int n = 0;}}
// CHECK: {{^\^}}

   "👿                                                              \z";

// CHECK: {{^  \.\.\.\\z";}}
// CHECK: {{^     \^~}}


    /* 👿 */ "👿berhund";

// CHECK: {{^    /\* <U\+1F47F> \*/ "<U\+1F47F>berhund";}}
// CHECK: {{^                    \^~~~~~~~~~~~~~~~~~}}

}
