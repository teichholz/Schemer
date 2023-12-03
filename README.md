# Schemer

Simple Scheme compiler. Trying to mostly mimic [R5RS](https://conservatory.scheme.org/schemers/Documents/Standards/R5RS/HTML/).

## Description
This repository contains the code for my bachelor thesis about a simple compiler for Scheme. The compiler is written in Haskell and makes a lot use of [Algebraic Data Types](https://wiki.haskell.org/Algebraic_data_type) for representing the AST in memory. 

It depends on the scheme runtime I have written: https://github.com/teichholz/Schemer-Runtime

The compiler uses multiple phases to break the code down into a core language which is easy to compile into [LLVM IR](https://llvm.org/docs/LangRef.html):
1. Parse S-Expressions: Parses the scheme code into generic symbolic expressions. Symbolic expressions are just expressions in [polish notation](https://en.wikipedia.org/wiki/Polish_notation), so I represent them like that in form of arbitrary deep nested lists. Can be found under: src/Sexp/Parser.hs.
2. Expand Macros: Next I expand Macros. Note that macros as defined in R5RS are not as sophisticated as they are today in the most used LISPs.
3. Parse into AST: Parses the macro expanded symbolic expressions into an AST which allows to identify all the relevant expressions like function application or function definition. Can be found under: src/Parser/ScSyn.hs
4. Toplevel Transformation: Removes all toplevel declarations of functions and variables. Uses a recursive let-binding to bind them into a single expressions which is further simplified in the next steps. Can be found under src/Phases/Toplevel.hs
5. Simplication: Simplifes certain scheme expressions. Can be found under src/Phases/Simplify.hs 
6. Mutation Removal: Removes all the places where variables are mutated by instead wrapping these variables in a one element vector. Can be found under src/Phases/Assignment.hs
7. Administrative Normal Form: Converts code into [ANF](https://en.wikipedia.org/wiki/A-normal_form). Can be found under src/Phases/ANF.hs
8. Continuation Passing Style: Converts code into [CPS](https://en.wikipedia.org/wiki/Continuation-passing_style). Can be found under src/Phases/CPS.hs
9. Unification of functions and function application: Removes all uses of apply by enforcing all functions to expect a single list as an argument. Can be found src/Phases/Unify.hs
10. Closure Conversion: Use [Closure Conversion](https://en.wikipedia.org/wiki/Lambda_lifting) to eliminate all free variables. Can be found under src/Phases/Closure.hs
11. Code Generation: Emits LLVM IR for the current AST. At this phase the core language consists of: literals, let-forms and variable access, if-expressions and function application. Function application differs between calling primitive functions and calling self-declared functions. Can be found under: src/Phases/Codegen.hs


## Installation
To run the compiler, make sure that nix is installed. Then first clone this repo:
```
git clone https://github.com/teichholz/Schemer && cd Schemer
```
After that you can run:
```
nix-shell && stack run -- -v
```
Which will throw you into a repl with verbose output enabled. 
