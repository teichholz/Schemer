//
// Created by tim on 12.05.20.
//


#include <cstdio>
#include "library.h"

extern "C" {
    int main(int argc, char** argv){
        init_gc();
        auto num = const_init_bool(true);
        display_aux(num);
        return 0;
    }
}