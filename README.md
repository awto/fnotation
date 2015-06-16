Function notation for Prolog
=============================

This is simple term_expansion/2 based macros for functions notation inspired 
by Mozart/Oz.

Use `$>` and `$<` operators in subterms of head or body to specify function's 
term and a variable placeholder respectively. If the variable placeholder isn't 
specified the library will add another (last) argument for the term. 

e.g.

    sample1(A,$> sample2($> sample3($<, A))) .

and 
    
    sample1(A,$> sample2($> sample3($<, A), $<)) .

and 

    sample1(A,R) :- sample2($> sample3($<, A), R) .

will be translated into 

    sample1(A,R) :- sample3(T,A) sample2(T, R) .


