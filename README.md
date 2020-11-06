Function notation for Prolog
=============================

This is simple term_expansion/2 based macros for functions notation.

This is a simple transform, the output is still easy to debug, 
it doesn't require special notation for function's definitions, 
any predicate may be used as a funciton already.

The simpliest usage is for predicate with the only output argument in the last 
position. But any predicate with any position of output arguments and any number 
of them may be called as a function. In case of more than one arguments only
one can be specified by placeholder but the others may still be bound with some
named variable.

Use `$>` and `$<` operators in subterms of head or body to specify function's
term and its output variable placeholder respectively. If the variable
placeholder isn't specified the library will add another (last) argument for
the term and use it as an output variable.

e.g.

    h1(A, $> h2()) :- p1(A, $> p2($> p3($<, A))) .

and
    
    h1(A, $> h2($<)) :- p1(A, $> p2($> p3($<, A), $<)) .

is converted into:

    h1(A,V1) :- h2(V1), p3(V0, A), p2(V0, V2), p1(A, V2) .

The operators names may be changed, for example making both of them to be `$$`:

    :- fnotation_ops($$,$$) .
    :- op(900,fx,$$) .

    h1(A,R) :- p1($$ p2($$, A), R) .


