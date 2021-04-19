//
// Created by tim on 12.05.20.
//


#include <cstdio>
#include "library.h"

extern "C" {
    int main(int argc, char** argv){
        init_gc();
        // auto num = const_init_int(2);
        // auto nil = get_nil();
        // auto cns = cons(num, nil);
        // auto vec = list2vector(cns);
        // auto clo = closure_create(vec);

        // auto iptr = closure_get_iptr(clo);
        // display(const_init_int(iptr));
        // display(cdr(cns));
        // halt(clo);

        auto nil = get_nil();
        auto cns = cons(const_init_int(1), cons(const_init_int(2), nil));
        halt(plus1(cns));

        return 0;
    }
}
