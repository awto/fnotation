Function notation for Prolog
=============================

This is simple term_expansion/2 based macros for functions notation inspired 
by Mozart/Oz.

Unlike other libraries for the same purposes this librart is much simplier, 
output code is still easy to debug, it doesn't require special notation for 
function's definitions, any predicate may be used as a funciton already. 
The simpliest usage is for predicate with the only output argument in the last 
position. But any predicate with any position of output arguments and any number 
of them may be called as a function. In case of more than one arguments only
one can be specified by placeholder but the others may still be bound with some
named variable.

Use `$>` and `$<` operators in subterms of head or body to specify function's 
term and its output variable placeholder respectively. If the variable 
placeholder isn't specified the library will add another (last) argument for 
the term and use it as output variable.

e.g.

    sample1(A,$> sample2($> sample3($<, A))) .

and 
    
    sample1(A,$> sample2($> sample3($<, A), $<)) .

and 

    sample1(A,R) :- sample2($> sample3($<, A), R) .

will be translated into 

    sample1(A,R) :- sample3(T,A) sample2(T, R) .

The operators name may be changed, for example making both of them to be `$$`:

    :- fnotation_ops($$,$$) .
    :- op(900,fx,$$) .

    sampled(A,R) :- sample2($$ sample3($$, A), R) .




