:- module(fnotation,[op(900,fx,$>),fnotation_ops/2]).

:- use_module(library(apply)).

:- dynamic in_op/1, out_op/1 .

%% fnoation_ops(+Atom,+Atom) is det
%
% defines operators for specifying function term and variable position in it
fnotation_ops(I,O) :-
        retractall(in_op(_)), retractall(out_op(_)),
        assertz(in_op(I)), assertz(out_op(O)) .

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
expand_notation_l(_,_,[Ftr,Arg],Var,GsH,GsT) :- in_op(Ftr), !,
        expand_notation(Var,B,Arg,Expr,GsH,(Res,GsT)),
        (var(B) *->
         Expr =.. [RFun|RArgs],
         append(RArgs,[Var],NRArgs),
         Res =.. [RFun|NRArgs]
        ; Res = Expr) .
expand_notation_l(Var,y,[Ftr],Var,Gs,Gs) :- out_op(Ftr), ! .
expand_notation_l(_,_,[H],H,Gs,Gs) :- ! .
expand_notation_l(Var,B,[H|Args],Res,GsH,GsT) :-
        foldl(expand_notation(Var,B),Args,ArgsRes,GsH,GsT),
        Res =.. [H|ArgsRes] . 


:- fnotation_ops($>,$<) .

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


