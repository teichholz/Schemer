#include "library.h"
#include <gc.h>
#include <cstdio>
#include <stdlib.h>
#include <cstring>


extern "C" {

//INIT
//

const char *get_type_of_sobj(SObj *obj) {
    const char *types[] = { "Nil", "Int", "Float", "Char", "String", "Bool", "Cons", "Symbol", "Vector", "Closure", "Undefined" };
    return types[obj->type];
}

void ASSERT_TYPE(SObj *obj, SType ty, const char* msg, const char *fn) {
    if (obj->type != ty) {
        type_errf(msg, fn, get_type_of_sobj(obj), obj);
    }
}

void init_gc() {
    GC_INIT();
}

SObj *alloc_atom() {
    return (SObj *) GC_MALLOC_ATOMIC(sizeof(SObj));
}

SObj *alloc() {
    return (SObj *) GC_MALLOC(sizeof(SObj));
}

void *alloc_mem(u64 bytes){
    return GC_MALLOC(bytes);
}

//HELPER

SObj *create_nil(){
    SObj *obj = alloc_atom();
    obj->type = Nil;
    return obj;
}

SObj *create_unspecified(){
    SObj *obj = alloc_atom();
    obj->type = Unspecified;
    return obj;
}

static SObj *nil = create_nil();
static SObj *unspecified = create_unspecified();
static SObj *f = const_init_bool(false);
static SObj *t = const_init_bool(true);
// Used for printing the type of Objects when encountering a type error

SObj *get_nil(){
    return nil;
}
SObj *get_unspecified(){
    return unspecified;
}
SObj *get_false(){
    return f;
}

SObj *get_true(){
    return t;
}

s64 _unwrap_int(SObj *int_obj) {
    return int_obj->content.Int.value;
}

s64 unwrap_int(SObj *int_obj, const char *fn_name) {
    ASSERT_TYPE(int_obj, Int, "%s: Expected Argument of type int", fn_name);
    return _unwrap_int(int_obj);
}

f64 _unwrap_float(SObj *float_obj) {
    return float_obj->content.Float.value;
}

f64 unwrap_float(SObj *float_obj, const char *fn_name) {
    ASSERT_TYPE(float_obj, Float, "%s: Expected Argument of type float", fn_name);
    return _unwrap_float(float_obj);
}


char _unwrap_char(SObj *char_obj) {
    return char_obj->content.Char.value;
}

char unwrap_char(SObj *char_obj, const char *fn_name) {
    ASSERT_TYPE(char_obj, Char, "%s: Expected Argument of type char", fn_name);
    return _unwrap_char(char_obj);
}

char* _unwrap_string(SObj *char_obj) {
    return char_obj->content.String.value;
}

char* unwrap_string(SObj *char_obj, const char *fn_name) {
    ASSERT_TYPE(char_obj, String, "%s: Expected Argument of type string", fn_name);
    return _unwrap_string(char_obj);
}

char* _unwrap_symbol(SObj *symbol_obj) {
    return symbol_obj->content.Symbol.value;
}

char* unwrap_symbol(SObj *symbol_obj, const char *fn_name) {
    ASSERT_TYPE(symbol_obj, Symbol, "%s: Expected Argument of type string", fn_name);
    return _unwrap_symbol(symbol_obj);
}

bool _unwrap_bool(SObj *bool_obj) {
    return bool_obj->content.Bool.value;
}

bool unwrap_bool(SObj *bool_obj, const char *fn_name) {
    ASSERT_TYPE(bool_obj, Bool, "%s: Expected Argument of type bool", fn_name);
    return _unwrap_bool(bool_obj);
}

SObj *_unwrap_vec(SObj *vec_obj) {
    return vec_obj->content.Vector.value;
}

SObj *unwrap_vec(SObj *vec_obj, const char *fn_name) {
    ASSERT_TYPE(vec_obj, Vector, "%s: Expected Argument of type vector", fn_name);
    return _unwrap_vec(vec_obj);
}


SObj *const_init_int(s64 val) {
    SObj *obj = alloc_atom();
    obj->type = Int;
    obj->content.Int.value = val;
    return obj;
}

SObj *const_init_float(f64 val) {
    SObj *obj = alloc_atom();
    obj->type = Float;
    obj->content.Float.value = val;
    return obj;
}

SObj *const_init_char(const char c) {
    SObj *obj = alloc_atom();
    obj->type = Char;
    obj->content.Char.value = c;
    return obj;
}

SObj *const_init_string(char *str) {
    SObj *obj = alloc();
    obj->type = String;
    obj->content.String.value = str;
    return obj;
}

SObj *const_init_symbol(char *str) {
    SObj *obj = alloc();
    obj->type = Symbol;
    obj->content.Symbol.value = str;
    return obj;
}

SObj *const_init_bool(const bool b) {
    SObj *obj = alloc_atom();
    obj->type = Bool;
    obj->content.Bool.value = b;
    return obj;
}

// closures
// frees =  array of SObj pointer
SObj *closure_create(SObj *vector){
    SObj *obj = alloc();
    obj->type = Closure;
    obj->content.Closure.vector = vector;
    return obj;
}
u64 closure_get_iptr(SObj *clo){
    auto vec = clo->content.Closure.vector;
    auto iptrobj = vector_ref(vec, const_init_int(0));
    return _unwrap_int(iptrobj);
}
SObj *closure_get_free(SObj *clo, SObj *index){
    auto vec = clo->content.Closure.vector;
    auto obj = vector_ref(vec, index);
    return obj;
}

bool both_of_type(SObj* obj1, SObj *obj2, Type type){{
    return obj1->type == type && obj2->type == type;
}}

SObj *sobj_of_bool(bool b){
    return b? t : f;
}

// conversion
one_arg_fun(symbol2string)
SObj *symbol2string(SObj *obj){
    return const_init_string(unwrap_symbol(obj, "symbol->string"));
}
one_arg_fun(string2symbol)
SObj *string2symbol(SObj *obj){
    return const_init_symbol(unwrap_string(obj, "string->symbol"));
}
one_arg_fun(vector2list)
SObj *vector2list(SObj *obj){
    auto vec = unwrap_vec(obj, "vector->list");
    int offset = obj->content.Vector.size - 1;
    if(offset < 0)
       fatal_errf("%s: vector has size 0!", "vector->list")

    SObj *first = vec;

    auto ptr = cons(first + offset, nil);
    for(int i = offset - 1; i >= 0; --i){
        ptr = cons(first + offset, ptr);
    }

    return ptr;
}
one_arg_fun(list2vector)
SObj *list2vector(SObj *obj){
    auto len = length(obj);
    ASSERT_TYPE(obj, Cons, "%s: Expected Argument of type List", "list->vector");
    auto vec = make_vector1(len);

    int i = 0;
    SObj *cons = obj;

    while(cons->type == Cons){
        vector_set(vec , const_init_int(i++), car(cons));
        cons = cdr(cons);
    }

    return vec;
}

// Predicates / booleans

SObj *not_bool(SObj *b) {
    return sobj_of_bool(b);
}

SObj *coerce(SObj *obj){
    return obj == f? f : t;
}

bool coerce_c(SObj *obj){
    return coerce(obj) == t;
}
// for types
one_arg_fun(consp)
SObj *consp(SObj* obj){
    return sobj_of_bool(obj->type == Cons);
}
one_arg_fun(nullp)
SObj *nullp(SObj* obj){
    //TODO
    return sobj_of_bool(obj->type == Nil);
}
one_arg_fun(listp)
SObj *listp(SObj* obj){
    if(obj->type == Nil)
        return sobj_of_bool(true);

    SObj *cons = obj;
    do{
        if(obj->type != Cons)
            return sobj_of_bool(false);

        cons = cdr(cons);
    } while(cons->type == Cons);

    return sobj_of_bool(cons->type == Nil);
}
one_arg_fun(vectorp)
SObj *vectorp(SObj* obj){
    return sobj_of_bool(obj->type == Vector);
}
one_arg_fun(booleanp)
SObj *booleanp(SObj* obj){
    return sobj_of_bool(obj->type == Bool);
}
one_arg_fun(procedurep)
SObj *procedurep(SObj* obj){
    return sobj_of_bool(obj->type == Closure);
}
one_arg_fun(numberp)
SObj *numberp(SObj* obj){
    return sobj_of_bool(obj->type == Int || obj->type == Float);
}
one_arg_fun(integerp)
SObj *integerp(SObj* obj){
    return sobj_of_bool(obj->type == Int);
}
one_arg_fun(floatp)
SObj *floatp(SObj* obj){
    return sobj_of_bool(obj->type == Float);
}
one_arg_fun(charp)
SObj *charp(SObj* obj){
    return sobj_of_bool(obj->type == Char);
}
one_arg_fun(symbolp)
SObj *symbolp(SObj* obj){
    return sobj_of_bool(obj->type == Symbol);
}
one_arg_fun(stringp)
SObj *stringp(SObj* obj){
    return sobj_of_bool(obj->type == String);
}

// for values
SObj *_cons_eq(SObj *obj1, SObj *obj2){
    if(equalp(length(obj1), length(obj2)) == f)
        return f;

    do{
        if(!eqp(car(obj1), car(obj2)))
            return f;

        obj1 = cdr(obj1);
        obj2 = cdr(obj2);
    }while( obj1->type == Cons );

    return t;
}

SObj *_vector_eq(SObj *vec1, SObj *vec2){
    if(equalp(vector_length(vec1), vector_length(vec2)) == t)
        return f;

    SObj *content1 = _unwrap_vec(vec1);
    SObj *content2 = _unwrap_vec(vec2);
    s64 size = _unwrap_int(content1);
    for(int i = 0; i < size ; ++i){
        if(equalp(content1 + i, content2 + i) == f){
            return f;
        }
    }

    return t;
}
two_arg_fun(stringeqp)
SObj *stringeqp(SObj *obj1, SObj *obj2){
    return sobj_of_bool(strcmp(unwrap_string(obj1, "string=?"), unwrap_string(obj2, "string=?")) == 0);
}
two_arg_fun(eqvp)
SObj *eqvp(SObj *obj1, SObj *obj2){
    if(obj1->type != obj2->type)
        return f;


    switch (obj1->type){
        case Int:
            return sobj_of_bool(_unwrap_int(obj1) == _unwrap_int(obj2));
        case Float:
            return sobj_of_bool(_unwrap_float(obj1) == _unwrap_float(obj2));
        case String:
            // compares adresses, as specified in r5rs
            return sobj_of_bool(_unwrap_string(obj1) == _unwrap_string(obj2));
        case Symbol:
            return stringeqp(symbol2string(obj1), symbol2string(obj2));
        case Char:
            return sobj_of_bool(_unwrap_char(obj1) == _unwrap_char(obj2));
        case Cons:
            //if(_unwrap_cons(obj1) == nil && _unwrap_cons(obj2) == nil)
            //    return sobj_of_bool(true);
            // general case of checkin if both are nil or both have the same adress
            return sobj_of_bool(obj1 == obj2);
        case Vector: // TODO
            return sobj_of_bool(obj1 == obj2);
    }
    // Simplified method, doesnt return unspecified, only true or false

    return f;
}
two_arg_fun(eqp)
SObj *eqp(SObj *obj1, SObj *obj2){
    return eqvp(obj1, obj2);
}
two_arg_fun(equalp)
SObj *equalp(SObj *obj1, SObj *obj2) {
    if (obj1->type != obj2->type)
        return f;

    switch (obj1->type){
        case String:
            // compares adresses, as specified in r5rs
            return stringeqp(obj1, obj2);
        case Cons:
            //if(_unwrap_cons(obj1) == nil && _unwrap_cons(obj2) == nil)
            //    return sobj_of_bool(true);
            // general case of checkin if both are nil or both have the same adress
            return _cons_eq(obj1, obj2);
        case Vector:
            return _vector_eq(obj1, obj2); // TODO
    }

    return eqp(obj1, obj2);
}

//char
two_arg_fun(chareqp)
SObj *chareqp(SObj *obj1, SObj *obj2) {
    return sobj_of_bool(unwrap_char(obj1, "char=?") == unwrap_char(obj2, "char=?"));
}
two_arg_fun(charltp)
SObj *charltp(SObj *obj1, SObj *obj2) {
    return sobj_of_bool(unwrap_char(obj1, "char<?") < unwrap_char(obj2, "char<?"));
}
two_arg_fun(chargtp)
SObj *chargtp(SObj *obj1, SObj *obj2) {
    return sobj_of_bool(unwrap_char(obj1, "char>?") > unwrap_char(obj2, "char>?"));
}
two_arg_fun(charltoep)
SObj *charltoep(SObj *obj1, SObj *obj2) {
    return sobj_of_bool(unwrap_char(obj1, "char<=?") <= unwrap_char(obj2, "char<=?"));
}
two_arg_fun(chargtoep)
SObj *chargtoep(SObj *obj1, SObj *obj2) {
    return sobj_of_bool(unwrap_char(obj1, "char>=?") >= unwrap_char(obj2, "char>=?"));
}

// string
one_arg_fun(make_string1)
SObj *make_string1(SObj *k) {
    u64 size = unwrap_int(k, "make-string");
    char* charseq = (char*) alloc_mem(sizeof(char) * size + 1); //include null-byte
    for(int i = 0; i < size; ++i)
        charseq[i] = '\ ';

    SObj *string = alloc();
    string->type = String;
    string->content.String.value = charseq;
    return string;
}
two_arg_fun(make_string2)
SObj *make_string2(SObj *k, SObj *fill) {
    SObj *string = make_string1(k);

    char fill_char = unwrap_char(fill, "make_string");
    char *content = _unwrap_string(string);
    for(int i = 0; content[i]; ++i){
        content[i] = fill_char;
    }
    return string;
}
one_arg_vafun(make_string)
SObj *make_string(SObj *list){
    if(_unwrap_int(length(list)) == 1)
        return apply_make_string1(list);
    else return  apply_make_string2(list);
}
one_arg_fun(string_length)
SObj *string_length(SObj *obj) {
    char *str = unwrap_string(obj, "string-length");
    return const_init_int(strlen(str));
}
two_arg_fun(string_ref)
SObj *string_ref(SObj *obj, SObj *k) {
    char* str = unwrap_string(obj, "string-ref");
    u64 index = unwrap_int(k, "string-ref");

    if(index >= 0 && index < strlen(str))
        return const_init_char(str[index]);
    else
    fatal_errf("%s: index is out of bounds!", "vector-ref")

}
three_arg_fun(string_set)
SObj *string_set(SObj *obj, SObj *k, SObj *ch) {
    char* str = unwrap_string(obj, "string-set");
    u64 index = unwrap_int(k, "string-set");
    char c = unwrap_char(ch, "string-set");

    if(index >= 0 && index < strlen(str))
        str[index] = c;
    else
    fatal_errf("%s: index is out of bounds!", "string-set")

    return unspecified;
}

//numbers


SObj *arhelper(SObj *obj1, SObj *obj2, const char op){
    SObj *ret;
    switch(obj2->type) {
        case Int:
            switch (obj1 -> type){
                case Int:
                    switch (op){
                        case '+':
                            ret = const_init_int(_unwrap_int(obj1) + _unwrap_int(obj2));
                            break;
                        case '-':
                            ret = const_init_int(_unwrap_int(obj1) - _unwrap_int(obj2));
                            break;
                        case '*':
                            ret = const_init_int(_unwrap_int(obj1) * _unwrap_int(obj2));
                            break;
                        case '/':
                            ret = const_init_float(_unwrap_int(obj1) / (float) _unwrap_int(obj2));
                            break;
                    }
                    break;
                case Float:
                    switch (op){
                        case '+':
                            ret = const_init_float(_unwrap_float(obj1) + _unwrap_int(obj2));
                            break;
                        case '-':
                            ret = const_init_float(_unwrap_float(obj1) - _unwrap_int(obj2));
                            break;
                        case '*':
                            ret = const_init_float(_unwrap_float(obj1) * _unwrap_int(obj2));
                            break;
                        case '/':
                            ret = const_init_float(_unwrap_float(obj1) / _unwrap_int(obj2));
                            break;
                    }
                    break;
            }
            break;
        case Float:
            switch (obj1->type){
                case Int:
                    switch (op){
                        case '+':
                            ret = const_init_float(_unwrap_int(obj1) + _unwrap_float(obj2));
                            break;
                        case '-':
                            ret = const_init_float(_unwrap_int(obj1) - _unwrap_float(obj2));
                            break;
                        case '*':
                            ret = const_init_float(_unwrap_int(obj1) * _unwrap_float(obj2));
                            break;
                        case '/':
                            ret = const_init_float(_unwrap_int(obj1) / _unwrap_float(obj2));
                            break;
                    }
                    break;
                case Float:
                    switch (op){
                        case '+':
                            ret = const_init_float(_unwrap_float(obj1) +  _unwrap_float(obj2));
                            break;
                        case '-':
                            ret = const_init_float(_unwrap_float(obj1) - _unwrap_float(obj2));
                            break;
                        case '*':
                            ret = const_init_float(_unwrap_float(obj1) *  _unwrap_float(obj2));
                            break;
                        case '/':
                            ret = const_init_float(_unwrap_float(obj1) /  _unwrap_float(obj2));
                            break;
                    }
            }
    }
    return ret;
}

SObj *afoldl(SObj *obj, SObj *init, const char op){
    if(listp(obj) == t){
        SObj *ptr = nullptr;
        SObj *cur = init;


        do{
            ptr = car(obj);
            if(numberp(ptr) == f) {
                fatal_errf("%s: Arguments must be numbers!", op)
            }

            obj = cdr(obj);
            cur = arhelper(cur, ptr, op);
        } while(obj->type == Cons);

        return cur;
    }

    fatal_errf("%s: expected a list!", "fold")
}
zero_arg_fun(plus0)
SObj *plus0(){
    return const_init_int(0);
}
one_arg_vafun(plus1)
SObj *plus1(SObj *obj){
    if (obj->type == Nil) return  plus0();
    return afoldl(obj, plus0(), '+');
}
zero_arg_fun(times0)
SObj *times0(){
    return const_init_int(1);
}
one_arg_vafun(times1)
SObj *times1(SObj *obj){
    if (obj->type == Nil) return  times0();
    return afoldl(obj, times0(), '*');
}
one_arg_vafun(minus1)
SObj *minus1(SObj *obj){
    SObj *first = car(obj);
    SObj *rest = cdr(obj);

    if(rest->type == Nil){
        switch (first->type){
            case Int:
                return const_init_int(- (_unwrap_int(first)));
            case Float:
                return const_init_float(- (_unwrap_float(first)));
        }
    }

    return afoldl(rest, first, '-');
}
one_arg_vafun(division1)
SObj *division1(SObj *obj){
    SObj *first = car(obj);
    SObj *rest = cdr(obj);

    if(rest->type == Nil){
        return first;
    }

    return afoldl(rest, first, '/');
}

int type_prec(SType type) {
    if(type == Int)
        return 1;
    if(type == Float)
        return 2;
    return 0;
}

bool lhelper(SObj *obj1, SObj *obj2, const char *op){
    switch(obj1->type){
        case Int:
            switch (obj2->type){
                case Float:
                    obj1 = const_init_float(_unwrap_int(obj1));
                    break;
            }
            break;
        case Float:
            switch (obj2->type){
                case Int:
                    obj2 = const_init_float(_unwrap_int(obj2));
                    break;
            }
            break;
    }

    if(strcmp(op, "=") == 0)
        return equalp(obj1, obj2) == t;


    if(strcmp(op, "<=") == 0)
        switch (obj1->type){
        case Float:
            return _unwrap_float(obj1) <= _unwrap_float(obj2);
        case Int:
            return _unwrap_int(obj1) <= _unwrap_int(obj2);
    }
    if(strcmp(op, ">=") == 0)
        switch (obj1->type){
            case Float:
                return _unwrap_float(obj1) >= _unwrap_float(obj2);
            case Int:
                return _unwrap_int(obj1) >= _unwrap_int(obj2);
        }
    if(strcmp(op, "<") == 0)
        switch (obj1->type) {
            case Float:
                return _unwrap_float(obj1) < _unwrap_float(obj2);
            case Int:
                bool b = _unwrap_int(obj1) < _unwrap_int(obj2);
                return b;
        }
    if(strcmp(op, ">") == 0)
        switch (obj1->type){
            case Float:
                return _unwrap_float(obj1) > _unwrap_float(obj2);
            case Int:
                return _unwrap_int(obj1) > _unwrap_int(obj2);
        }
}

SObj *lfoldl(SObj *obj, SObj *init, const char* op){
    if(listp(obj) == t){
        SObj *ptr = init;
        SObj *cur = init;

        do{
            ptr = car(obj);
            if(numberp(ptr) == f) {
                fatal_errf("%s: Arguments must be numbers!", op)
            }
            if(!lhelper(cur, ptr, op))
                return f;

            cur = ptr;
            obj = cdr(obj);
        } while(obj->type == Cons);

        return t;
    }

    fatal_errf("%s: expected a list!", op)
}
one_arg_vafun(eqsign)
SObj *eqsign(SObj *list){
    SObj *first = car(list);
    SObj *rest = cdr(list);

    if(rest->type == Nil){
        return sobj_of_bool(first);
    }

    return lfoldl(rest, first, "=");
}
one_arg_vafun(ltsign)
SObj *ltsign(SObj *list){
    SObj *first = car(list);
    SObj *rest = cdr(list);

    if(rest->type == Nil){
        return sobj_of_bool(first);
    }

    return lfoldl(rest, first, "<");
}
one_arg_vafun(ltoesign)
SObj *ltoesign(SObj *list){
    SObj *first = car(list);
    SObj *rest = cdr(list);

    if(rest->type == Nil){
        return sobj_of_bool(first);
    }

    return lfoldl(rest, first, "<=");
}
one_arg_vafun(gtsign)
SObj *gtsign(SObj *list){
    SObj *first = car(list);
    SObj *rest = cdr(list);

    if(rest->type == Nil){
        return sobj_of_bool(first);
    }

    return lfoldl(rest, first, ">");
}
one_arg_vafun(gtoesign)
SObj *gtoesign(SObj *list){
    SObj *first = car(list);
    SObj *rest = cdr(list);

    if(rest->type == Nil){
        return sobj_of_bool(first);
    }

    return lfoldl(rest, first, ">=");
}

//cons
two_arg_fun(cons)
SObj *cons(SObj* car, SObj *cdr){
    SObj *cons = alloc();
    cons->type = Cons;
    cons->content.Cons.car = alloc();
    cons->content.Cons.cdr = alloc();
    *cons->content.Cons.car = *car;
    *cons->content.Cons.cdr = *cdr;
    return cons;
}

SObj *_car(SObj *cons){
    return cons->content.Cons.car;
}

SObj *_cdr(SObj *cons){
    return cons->content.Cons.cdr;
}
one_arg_fun(car)
SObj *car(SObj *cons){
    ASSERT_TYPE(cons, Cons, "%s: Expected Argument of type cons", "car");
    return cons->content.Cons.car;
}
one_arg_fun(cdr)
SObj *cdr(SObj *cons){
    ASSERT_TYPE(cons, Cons, "%s: Expected Argument of type cons", "cdr");
    return cons->content.Cons.cdr;
}
two_arg_fun(set_car)
SObj *set_car(SObj *cons, SObj *car){
    ASSERT_TYPE(cons, Cons, "%s: Expected Argument of type cons", "set-car!");
    *cons->content.Cons.car = *car;
    return unspecified;
}
two_arg_fun(set_cdr)
SObj *set_cdr(SObj *cons, SObj *cdr){
    ASSERT_TYPE(cons, Cons, "%s: Expected Argument of type cons", "set-cdr!");
    *cons->content.Cons.cdr = *cdr;
    return unspecified;
}
// (1 . (2 . (3 . 4)))
one_arg_fun(length)
SObj *length(SObj *obj){
    if(listp(obj) == f) {
        fatal_errf("%s: Expected Argument of type list", "length")
    }
    if(obj->type == Nil) return const_init_int(0);

    SObj *cons = obj;
    s64 length = 0;
    do{
        ++length;
        cons = cdr(cons);
    } while(cons->type == Cons);

    return const_init_int(length);
}
two_arg_fun(memv)
SObj *memv(SObj *obj, SObj *list){
    ASSERT_TYPE(list, Cons, "%s: Expected Argument of type Cons", "memv");

    auto ptr = list;

    while(ptr->type != Nil){
        if(eqvp(car(ptr), obj) == t)
            return ptr;
        ptr = cdr(ptr);
    }
    return f;
}

// Vector
one_arg_fun(make_vector1)
SObj *make_vector1(SObj *k) {
    s64 size = unwrap_int(k, "make-vector");
    SObj *content = (SObj*) alloc_mem(size * sizeof(SObj));
    SObj *vec = alloc();
    vec->type = Vector;
    vec->content.Vector.size = size;
    vec->content.Vector.value = content;
    return vec;
}
two_arg_fun(make_vector2)
SObj *make_vector2(SObj *k, SObj *fill) {
    SObj *vec = make_vector1(k);
    s64 size = vec->content.Vector.size;
    for(int i = 0; i < size ; ++i){
        vec->content.Vector.value[i] = *fill;
    }
    return vec;
}
// one_arg_vafun(make_vector)
// SObj *make_vector(SObj *list){
//     if(_unwrap_int(length(list)) == 1)
//         return apply_make_vector1(list);
//     else return  apply_make_vector2(list);
// }

one_arg_fun(vector_length)
SObj *vector_length(SObj *vec) {
    return const_init_int(vec->content.Vector.size);
}
two_arg_fun(vector_ref)
SObj *vector_ref(SObj *vec, SObj *k) {
    SObj *clo_or_vec;
    if (vec->type == Closure) {
        clo_or_vec = vec->content.Closure.vector;
    } else {
        clo_or_vec = vec;
    }

    SObj *content = unwrap_vec(clo_or_vec, "vector-ref");
    s64 index = unwrap_int(k, "vector-ref");
    if(index >= 0 && index < clo_or_vec->content.Vector.size)
        return content + index;
    else
    fatal_errf("%s: index is out of bounds!", "vector-ref")
}
three_arg_fun(vector_set)
SObj *vector_set(SObj *vec, SObj *k, SObj *obj) {
    SObj *content = unwrap_vec(vec, "vector-set");
    s64 index = unwrap_int(k, "vector-set");
    if(index >= 0 && index < vec->content.Vector.size)
        content[index] = *obj;
    else
    fatal_errf("%s: index is out of bounds!", "vector-set")

    return unspecified;
}
one_arg_fun(add1)
SObj *add1(SObj *obj) {
    return const_init_int(unwrap_int(obj, "add1") + 1);
}

SObj *add(SObj *obj1, SObj *obj2) {
    return const_init_int(unwrap_int(obj1, "add") + unwrap_int(obj2, "add"));
}
one_arg_fun(display)
SObj *display(SObj *obj){
    display_aux(obj);
    printf("\n");
    return unspecified;
}

one_arg_fun(halt)
SObj *halt(SObj *obj){
    display(obj);
    exit(0);
    return obj;
}

// display / printing

void display_aux(SObj *obj) {
    switch (obj->type) {
        case Int:
            printf("%ld", _unwrap_int(obj));
            break;
        case Float:
            printf("%f", _unwrap_float(obj));
            break;
        case Char:
            printf("#\\%c", _unwrap_char(obj));
            break;
        case String:
            printf("%s", _unwrap_string(obj));
            break;
        case Symbol:
            printf("%s", _unwrap_symbol(obj));
            break;
        case Bool:
            printf("%s", (_unwrap_bool(obj))? "#t" : "#f");
            break;
        case Unspecified:
            printf("%s", "unspecified");
            break;
        case Cons:
            display_cons(obj);
            break;
        case Nil:
            printf("'()");
            break;
        case Vector:
            display_vec(obj);
            break;
        case Closure:
            printf("<closure>");
            break;
    }
}

// (cons 1 (cons 2 (cons 3 '())))
// (cons 1 (cons 2 3))
SObj *display_cons(SObj *cons) {
    SObj *first = car(cons);
    SObj *rest = cdr(cons);

    printf("'(");
    display_aux(first);
    while(rest->type == Cons){
        first = car(rest);
        rest = cdr(rest);

        printf(" ");
        display_aux(first);
    }

    if(rest->type != Nil){
        printf(" . ");
        display_aux(rest);
    }
    printf(")");
    return unspecified;
}

SObj *display_vec(SObj *vec){
    u64 size = vec->content.Vector.size;
    SObj *contents = _unwrap_vec(vec);
    printf("#(");
    for(int i = 0; i < size; ++i){
        display_aux(contents + i);
        if(i < size - 1)
            printf(" ");
    }
    printf(")");
    return unspecified;
}

}


