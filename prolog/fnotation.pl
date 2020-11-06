:- module(fnotation,[op(900,fx,$>),fnotation_ops/2]).

:- use_module(library(apply)).

:- dynamic in_op/1, out_op/1, mpred_memo/2 .

%% fnoation_ops(+Atom,+Atom) is det
%
% defines operators for specifying function term and variable position in it
fnotation_ops(I, O) :-
  retractall(in_op(_)), retractall(out_op(_)),
  assertz(in_op(I)), assertz(out_op(O)) .

expand_notation(_, _, Var, Var, Gs, Gs) :- var(Var), ! .
expand_notation(Var, y, Term, Var, Gs, Gs) :- out_op(Term), ! .
expand_notation(Var, HasOut, Term, ResTerm, GsH, GsT) :-
  compound(Term),!,
  compound_name_arguments(Term, Ftr, Args),
  (predicate_property(Term, meta_predicate(_))
   *->
     maplist(expand_notation_term, Args, ResArgs),
     compound_name_arguments(ResTerm, Ftr, ResArgs),
     GsH = GsT
  ; in_op(Ftr), [Arg] = Args
    *-> 
    expand_notation(ResTerm, SubHasOut, Arg, ResArg, GsH, (SubTerm, GsT)),
    (var(SubHasOut), compound(ResArg)
     *->
       compound_name_arguments(ResArg, RFtr, RArgs),
       append(RArgs, [ResTerm], NRArgs),
       compound_name_arguments(SubTerm, RFtr, NRArgs)
    ; SubTerm = ResArg)
  ; foldl(expand_notation(Var, HasOut), Args, ArgsRes, GsH, GsT),
    compound_name_arguments(ResTerm, Ftr, ArgsRes)
  )
.
expand_notation(_, _, Term, Term, Gs, Gs).

expand_notation_term(Term, Res) :-
  expand_notation(_, _, Term, Tmp, Res, Tmp) .

:- fnotation_ops($>, $<) .

fn_expand(H :- B, RH :- NB) :-
  expand_notation(_, _, B, RB, Gs, RB),
  expand_notation(_, _, H, RH, NB, Gs),
  (H :- B) \= (RH :- NB), ! .
fn_expand(H, R) :- 
  not(functor(H,:-,2)), fn_expand(H :- true,R) . 

:- begin_tests(fn_expand).

test(fn_expand1, [true(Ret = (h1 :- (p3(V1, A), p2(V1, V2), p1(A, V2))))]) :- 
  fn_expand((h1 :- p1(A, $> p2($> p3($<, A)))), Ret) .

test(fn_expand2, [true(Ret = (h1(A, R) :- p2(V1, A), p1(V1, R)))]) :- 
  fn_expand((h1(A, R) :- p1($> p2($<, A), R)), Ret).

test(fn_expand_fails, [fail]) :- 
  fn_expand((h1(A, R) :- p1(p2(A), R)), _).

test(fn_expand_metapreds,
     [true(Ret = (h1(A, B) :- p1, ((p3(V1, B), p2(V1))->(p5(A, V2), p4(V2));(p7(V3), p6(V3)))))]) :- 
  fn_expand((h1(A,B) :- p1, (p2($> p3($<,B)) -> p4($>p5(A));p6($>p7()))), Ret).

test(fn_expand_head1, [true(Ret = (h1(V1) :- b(V1), p1))]) :-
  fn_expand((h1($> b()) :- p1), Ret).

test(fn_expand_head2, [true(Ret = (h1(V1) :- c(A, V2), b(V2, V1), p1(A)))]) :-
  fn_expand((h1($> b($> c(A,$<))) :- p1(A)), Ret).

test(fn_expand_head_with_body, [true(Ret = (h1(V1) :- h2(V1), p2(V2), p1(V2)))]) :-
  fn_expand((h1($> h2()) :- p1($> p2())), Ret).

test(fn_expand_head_with_body2,
     [true(Ret = (h1(V1, V2, C) :- h2(V1, A), h3(B, V2), p2(p3(V3, B)), p4(C, V4), p1(A, V3, V4)))]) :-
  fn_expand((h1($> h2($<, A), $> h3(B), C) :- p1(A, $> p2(p3($<, B)), $>p4(C))), Ret).

:- op(900, fx, $$).    

test(fn_expand_custom, [
       setup(fnotation_ops($$,$$)),
       cleanup(fnotation_ops($>,$<)),
       true(Ret = (h1(A, R):-p2(V1, A), p1(V1, R)))]) :- 
  fn_expand((h1(A,R) :- p1($$ p2($$, A), R)), Ret).

:- end_tests(fn_expand).

:- multifile user:term_expansion/2.
:- dynamic   user:term_expansion/2.

user:term_expansion(F, R) :- fn_expand(F, R) .

:- begin_tests(fnotation).

pred1(A,s(A)) .
pred2 :- pred1(a, $> pred1(a)) .
pred3($> pred1(a)).

test(run_pred1) :- pred2 .
test(run_pred2) :- pred3(s(a)) .

:- op(900, fx, $$).    

test(fn_expand_custom, [
       setup(fnotation_ops($$,$$)),
       cleanup(fnotation_ops($>,$<)),
       true(Ret = (h1(A, R):-p2(V1, A), p1(V1, R)))]) :- 
  fn_expand((h1(A,R) :- p1($$ p2($$, A), R)), Ret).

:- end_tests(fnotation).
