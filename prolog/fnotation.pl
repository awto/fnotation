:- module(fnotation,[op(900,fx,$>)]).

:- use_module(library(apply)).

subterm(T,T).
subterm(T,S) :-
        T =.. [_|Args],
        member(I,Args),
        subterm(I,S) .

expand_notation(_,_,Var,Var,Gs,Gs) :- var(Var), ! .
expand_notation(Var,B,Term,ResTerm,GsH,GsT) :-
        Term =.. LTerm,
        expand_notation_l(Var,B,LTerm,ResTerm,GsH,GsT).
expand_notation_l(Var,B,[',',Arg1,Arg2],Res,Gs,Gs) :- !,
        expand_notation(Var,B,Arg1,Res1,Res,(Res1,Tmp)),
        expand_notation(Var,B,Arg2,Res2,Tmp,Res2) .
expand_notation_l(_,_,[$>,Arg],Var,GsH,GsT) :- !,
        expand_notation(Var,B,Arg,Expr,GsH,(Res,GsT)),
        (var(B) *->
         Expr =.. [RFun|RArgs],
         append(RArgs,[Var],NRArgs),
         Res =.. [RFun|NRArgs]
        ; Res = Expr) .
expand_notation_l(Var,y,[$<],Var,Gs,Gs) :- ! .
expand_notation_l(_,_,[H],H,Gs,Gs) :- ! .
expand_notation_l(Var,B,[H|Args],Res,GsH,GsT) :-
        foldl(expand_notation(Var,B),Args,ArgsRes,GsH,GsT),
        Res =.. [H|ArgsRes] . 

fn_expand(H :- B, RH :- NB) :-
  expand_notation(_,_,B,RB,NB,Gs),
  expand_notation(_,_,H,RH,Gs,RB),
  (H :- B) \= (RH :- NB)
  .

fn_expand(H,R) :- 
  not(functor(H,:-,2)), fn_expand(H:-true,R) . 

:- multifile system:term_expansion/2.
:- dynamic   system:term_expansion/2.

system:term_expansion(F,T) :- fn_expand(F,T) .


