#ifndef RUNTIME_LIBRARY_H
#define RUNTIME_LIBRARY_H


#define fatal_err(format) { \
        printf("Run-time error: "); \
        printf(format); \
        printf("\n"); \
        exit(1); \
        }

#define fatal_errf(format, fn) { \
        printf("Runtime error: "); \
        printf(format, fn); \
        printf("\n"); \
        exit(1); \
        }

#define type_errf(format, fn, actualType, obj) { \
        printf("Runtime type error: "); \
        printf(format, fn); \
        printf("\n"); \
        printf("But got: %s\n", actualType); \
        printf("With value: "); \
        display(obj); \
        exit(1); \
        }

#define zero_arg_fun(fn) \
        SObj* apply_##fn(){ \
        return fn(); \
}
#define one_arg_fun(fn) \
        SObj* apply_##fn(SObj* obj){ \
        auto car42 = _car(obj);  \
        return fn(car42); \
}
#define one_arg_vafun(fn) \
        SObj* apply_##fn(SObj* obj){ \
        return fn(obj); \
}

#define two_arg_fun(fn) \
        SObj* apply_##fn(SObj* obj){ \
        auto car42 = _car(obj);  \
        auto cadr42 = _car(_cdr(obj));  \
        return fn(car42, cadr42); \
}

#define three_arg_fun(fn) \
        SObj* apply_##fn(SObj* obj){ \
        auto car42 = _car(obj);  \
        auto cadr42 = _car(_cdr(obj));  \
        auto caddr42 = _car(_cdr(_cdr(obj)));  \
        return fn(car42, cadr42, caddr42); \
}


#include <cstdint>
#include <string>

extern "C" {

typedef uint64_t u64;
typedef int64_t s64;
typedef _Float64 f64;

typedef enum Type {
    Nil, Int, Float, Char, String, Bool, Cons, Symbol, Vector, Closure, Unspecified
} SType;

// Possible Options:
// Type punning (bad), tagged unions (c), variants (c++)
typedef struct SObj {
    SType type;
    union {
        struct {
            s64 value;
        } Int;
        struct {
            f64 value;
        } Float;
        struct {
            char value;
        } Char;
        struct {
            char* value;
        } String;
        struct {
            bool value;
        } Bool;
        struct {
            SObj *car;
            SObj *cdr;
        } Cons;
        struct {
            char *value;
        } Symbol;
        struct {
            int size;
            SObj *value;
        } Vector;
        struct {
            u64 iptr;
            SObj *vector;
        } Closure;
    } content;
} SObj;

// init
void init_gc();
SObj *alloc_atom();
SObj *alloc();
void *alloc_mem(u64);
SObj *const_init_int(long);
SObj *const_init_float(f64);
SObj *const_init_char(char);
SObj *const_init_string(char*);
SObj *const_init_bool(bool);

// closures
SObj *closure_create(SObj *);
u64 closure_get_iptr(SObj *);
SObj* closure_get_frees(SObj *);

/*
let list = ["="; ">"; "<"; "<="; ">="; "+"; "-"; "*"; "/";
"cons?"; "null?"; "cons"; "car"; "cdr"; "list";
"length";
"vector?"; "vector"; "make-vector"; "vector-ref"; "vector-set!"; "vector-length";
"list?"; "void?"; "promise?"; "procedure?"; "number?"; "integer?";
"error"; "void"; "print"; "println"; "display"; "write"; "exit"; "halt";
"eq?"; "eqv?"; "equal?"; "not";] in
*/




//helper, mostly exported for testing
SObj *arhelper(SObj*, SObj*, const char);
s64 unwrap_int(SObj*, const char*);
s64 _unwrap_int(SObj*);
f64 unwrap_float(SObj*, const char*);
f64 _unwrap_float(SObj*);
char unwrap_char(SObj*, const char*);
char* unwrap_string(SObj*, const char*);
char* unwrap_symbol(SObj*, const char*);
bool unwrap_bool(SObj*, const char*);
SObj *unwrap_cons(SObj*, const char*);
SObj *unwrap_vec(SObj*, const char*);
void display_aux(SObj *obj);
SObj *display(SObj *obj);
SObj *display_cons(SObj *obj);
SObj *display_vec(SObj *obj);
SObj *halt(SObj*);

SObj *create_nil();
SObj *get_nil();
SObj *get_unspecified();
SObj *get_false();
SObj *get_true();

//prims
//predicates
SObj *consp(SObj*);
SObj *nullp(SObj*); //true if empty-list else false
SObj *listp(SObj*);
SObj *vectorp(SObj*);
SObj *procedurep(SObj*);
SObj *numberp(SObj*);
SObj *charp(SObj*);
SObj *booleanp(SObj*);
SObj *symbolp(SObj*);
SObj *stringp(SObj*);
SObj *integerp(SObj*);
SObj *floatp(SObj*);
SObj *eqp(SObj*, SObj*);
SObj *eqvp(SObj*, SObj*);
SObj *equalp(SObj*, SObj*);
SObj *stringeqp(SObj*, SObj*);

//conversion
SObj *string2symbol(SObj*);
SObj *symbol2string(SObj*);
SObj *list2vector(SObj*);
SObj *vector2list(SObj*);

//char
SObj *chareqp(SObj *, SObj *);
SObj *charltp(SObj *, SObj *);
SObj *chargtp(SObj *, SObj *);
SObj *charltoep(SObj *, SObj *);
SObj *chargtoep(SObj *, SObj *);

//numbers
SObj *eqsign(SObj*);
SObj *ltsign(SObj*);
SObj *gtsign(SObj*);
SObj *ltoesign(SObj*);
SObj *gtoesign(SObj*);
SObj *add1(SObj*);
SObj *plus0();
SObj *plus1(SObj*);
SObj *times0();
SObj *times1(SObj*);
SObj *minus1(SObj*);
SObj *division1(SObj*);

//string
SObj *make_string(SObj *);
SObj *make_string1(SObj *);
SObj *make_string2(SObj *, SObj *);
SObj *string_length(SObj *);
SObj *string_ref(SObj *, SObj *);
SObj *string_set(SObj *, SObj *, SObj *);


// cons \ list
SObj *cons(SObj*, SObj*);
SObj *car(SObj*);
SObj *cdr(SObj*);
SObj *_car(SObj*);
SObj *_cdr(SObj*);
SObj *length(SObj*);
SObj *set_car(SObj *, SObj *);
SObj *set_cdr(SObj *, SObj *);
SObj *memv(SObj *, SObj *);

// vector
SObj *make_vector(SObj *);
SObj *make_vector1(SObj *);
SObj *make_vector2(SObj *, SObj *);
SObj *vector_length(SObj *);
SObj *vector_ref(SObj *, SObj *);
SObj *vector_set(SObj *, SObj *, SObj *);

// boolean
SObj *not_bool(SObj *); // not
SObj *coerce(SObj *);
bool coerce_c(SObj *);
};



#endif //RUNTIME_LIBRARY_H
