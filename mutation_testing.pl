% (c) 2018 Lehrstuhl fuer Softwaretechnik und Programmiersprachen,
% Heinrich Heine Universitaet Duesseldorf
% This software is licenced under EPL 1.0 (http://www.eclipse.org/org/documents/epl-v10.html)

:- module(mutation_testing, [mutation_test/0, mutation_test/1, mutation_test/2]).

:- if(current_prolog_flag(dialect, sicstus)).

:- use_module(library(timeout), [time_out/3]).
:- use_module(library(lists)).
:- use_module(library(terms)).
:- dynamic test_failed/0.

:- endif.

:- dynamic infinite_loop/0.
:- dynamic current_tests/1.
:- dynamic has_cut/2.
:- dynamic my_clause/3.
:- dynamic seen/3.
:- dynamic my_test/3.
:- dynamic redefined_instance/4.
:- dynamic mutation_result/5.

basic(remove_all(_)).
basic(disjunction_to_conjunction(_)).
basic(unif_equal_to_unif_nonequal(_)).
basic(unif_nonequal_to_unif_equal(_)).
basic(arith_equal_to_arith_nonequal(_)).
basic(arith_nonequal_to_arith_equal(_)).
basic(equal_to_nonequal(_)).
basic(nonequal_to_equal(_)).
basic(negate_gt(_)).
basic(negate_lt(_)).
basic(negate_ge(_)).
basic(negate_le(_)).
basic(edge_gt(_)).
basic(edge_lt(_)).
basic(edge_ge(_)).
basic(edge_le(_)).
basic(add_to_sub(_)).
basic(sub_to_add(_)).
basic(mul_to_add(_)).
basic(div_to_sub(_)).
basic(negate_expression(_)).
basic(mutate_arity0(_, variable_to_anonymous)).
basic(mutate_arity0(_, true_to_false)).
basic(mutate_arity0(_, false_to_true)).
basic(mutate_arity0(_, atom_to_anonymus)).
basic(mutate_arity0(_, empty_list_to_anonymous)).

deep(Mutation) :- basic(Mutation).
deep(permute_cut(_)).
deep(mutate_arity0(_, increase_number)).
deep(mutate_arity0(_, decrease_number)).

experimental(Mutation) :- deep(Mutation).
experimental(conjunction_to_disjunction(_)).
experimental(reverse_predicate(_)).

% LAUNCH PREDICATE
mutation_test :-
  mutation_test([], basic), !.
mutation_test(basic) :-
  mutation_test([], basic), !.
mutation_test(deep) :-
  mutation_test([], deep), !.
mutation_test(experimental) :-
  mutation_test([], experimental), !.
mutation_test(Modules) :-
  is_list(Modules),
  mutation_test(Modules, basic), !.
mutation_test(_, []) :-
  !, format('~nPlease specify the desired mutations. ~n~n', []).
mutation_test(_, _) :-
  \+ my_clause(_, _, _),
  !, format('~nThere are no predicates to be tested. ~n~n', []).

:- if(current_prolog_flag(dialect, swi)).

mutation_test(ModuleOptions, Depth) :-
  cleanup,
  ground(ModuleOptions),
  find_mutations(Depth, Mutations),
  format('~nRunning tests without any mutation: ~n~n', []),
  my_run(ModuleOptions, Tests),
  (Tests = []-[] -> format('There are no tests declared for the specified module(s) ~n', []), fail; true),
  statistics(walltime, [Start|_]),
  my_run_tests(Tests),
  statistics(walltime, [End|_]),
  calc_cap(1, Start, End, Cap),
  ansi_format([bold,fg(green)], '~nOK~n~n', []),
  set_my_stream,
  format('Starting to mutate source code: ~n~n', []),
  member(KindOfChange, Mutations),
  mutate(KindOfChange, ModuleOptions, Implementations, NewImplementations, mutation),
  prepare_result(KindOfChange, Predicate, Mutation),
  retractall(infinite_loop),
  (
    catch(call_with_time_limit(Cap, my_run_tests(Tests)), Exception, swi_exception_handler(Exception)) ->
      assertz(mutation_result(Predicate, Mutation, Implementations, NewImplementations, alive)),
      format('~nMessage: ~nTests did not fail after mutation! ~n~nPredicate: ~n~w ~n~nMutation: ~n~w~n', [Predicate, Mutation]),
      ansi_format([bold,fg(red)], '~nWARNING~n~n', [])
      ;
      (
        \+ infinite_loop ->
          assertz(mutation_result(Predicate, Mutation, Implementations, NewImplementations, dead)),
          format('~nMessage: ~nAt least a single test has failed! ~n~nPredicate: ~n~w ~n~nMutation: ~n~w~n', [Predicate, Mutation]),
          ansi_format([bold,fg(green)], '~nOK~n~n', [])
          ;
          assertz(mutation_result(Predicate, Mutation, Implementations, NewImplementations, loop)),
          format('~nMessage: ~nAn infinite loop occurred! ~n~nPredicate: ~n~w ~n~nMutation: ~n~w~n', [Predicate, Mutation]),
          ansi_format([bold,fg(yellow)], '~nNEUTRAL~n~n', [])
      )
  ),
  ansi_format([bold,fg(blue)], '%%% ... %%%~n~n', []),
  fail.


:- elif(current_prolog_flag(dialect, sicstus)).

mutation_test(ModuleOptions, Depth) :-
  cleanup,
  ground(ModuleOptions),
  find_mutations(Depth, Mutations),
  format('~nRunning tests without any mutation: ~n~n', []),
  my_run(ModuleOptions, Tests),
  (Tests = []-[] -> format('~nThere are no tests declared for the specified module(s) ~n', []), fail; true),
  statistics(walltime, [Start|_]),
  my_run_tests(Tests),
  statistics(walltime, [End|_]),
  calc_cap(1000, Start, End, Cap),
  format('~nOK~n~nStarting to mutate source code: ~n~n', []),
  member(KindOfChange, Mutations),
  mutate(KindOfChange, ModuleOptions, Implementations, NewImplementations, mutation),
  prepare_result(KindOfChange, Predicate, Mutation),
  retractall(test_failed),
  retractall(infinite_loop),
  time_out(my_run_tests(Tests), Cap, Result),
  (Result == time_out -> asserta(infinite_loop); true),
  (
    \+ test_failed ->
      (
        \+ infinite_loop ->
          assertz(mutation_result(Predicate, Mutation, Implementations, NewImplementations, alive)),
          format('~nMessage: ~nTests did not fail after mutation! ~n~nPredicate: ~n~w ~n~nMutation: ~n~w~n', [Predicate, Mutation]),
          format('~nWARNING ~n~n', [])
          ;
          assertz(mutation_result(Predicate, Mutation, Implementations, NewImplementations, loop)),
          format('~nMessage: ~nAn infinite loop occurred! ~n~nPredicate: ~n~w ~n~nMutation: ~n~w~n', [Predicate, Mutation]),
          format('~nNEUTRAL ~n~n', [])
      )
      ;
      assertz(mutation_result(Predicate, Mutation, Implementations, NewImplementations, dead)),
      format('~nMessage: ~nAt least a single test has failed! ~n~nPredicate: ~n~w ~n~nMutation: ~n~w~n', [Predicate, Mutation]),
      format('~nOK ~n~n', [])
  ),
  format('%%% ... %%% ~n~n', []),
  fail.

:- endif.

mutation_test(_, _) :-
  findall(mutation_result(Predicate, Mutation, Implementations, NewImplementations, alive), mutation_result(Predicate, Mutation, Implementations, NewImplementations, alive), LivingMutants),
  findall(mutation_result(Predicate, Mutation, Implementations, NewImplementations, dead), mutation_result(Predicate, Mutation, Implementations, NewImplementations, dead), DeadMutants),
  findall(mutation_result(Predicate, Mutation, Implementations, NewImplementations, loop), mutation_result(Predicate, Mutation, Implementations, NewImplementations, loop), LoopMutants),
  length(LivingMutants, LivingCount),
  length(DeadMutants, DeadCount),
  length(LoopMutants, LoopCount),
  Mutations is LivingCount + DeadCount,
  AllMutations is Mutations + LoopCount,
  print_result(DeadCount, LivingCount, LoopCount, AllMutations, Mutations).
  %export(LivingMutants, DeadMutants, LoopMutants, LivingCount, DeadCount, LoopCount, AllMutations, Mutations).

mutate_aux(Implementations, [], MutationOrFix) :-
  format('Source: ~n~w~n~n', [Implementations]),
  format('Mutant: ~n[]~n~n', []),
  Implementations = [Implementation|_],
  retractall_clause(Implementation),
  MutationOrFix = mutation.

mutate_aux(Implementations, [], MutationOrFix) :- !,
  maplist(assert_clause, Implementations),
  MutationOrFix = fix.

mutate_aux(Implementations, NewImplementations, MutationOrFix) :-
  format('Source: ~n~w~n~n', [Implementations]),
  format('Mutant: ~n~w~n~n', [NewImplementations]),
  Implementations = [Implementation|_], % DANGER: if NewImplementations contains Implementation
  retractall_clause(Implementation),    % with other arity than Implementations it will explode again
  maplist(assert_clause, NewImplementations),
  MutationOrFix = mutation.

mutate_aux(Implementations, NewImplementations, MutationOrFix) :-
  NewImplementations = [NewImplementation|_],
  retractall_clause(NewImplementation),
  maplist(assert_clause, Implementations),
  MutationOrFix = fix.

pred_iterator([], Functor, Implementations) :- !,
  seen(Module, Name, Arity),
  functor(Functor, Name, Arity),
  findall(my_clause(Module, Functor, Body), clause(Module:Functor, Body), Implementations).

pred_iterator(Options, Functor, Implementations) :-
  seen(Module, Name, Arity),
  my_member(Module, Options),
  functor(Functor, Name, Arity),
  findall(my_clause(Module, Functor, Body), clause(Module:Functor, Body), Implementations).

pred_iterator([], Functor, AllImplementations, CutImplementations) :- !,
  seen(Module, Name, Arity),
  functor(Functor, Name, Arity),
  findall((my_clause(Module, Functor, Body), Positions), has_cut(my_clause(Module, Functor, Body), Positions), CutImplementations),
  CutImplementations \= [],
  findall(my_clause(Module, Functor, Body), clause(Module:Functor, Body), AllImplementations).

pred_iterator(Options, Functor, AllImplementations, CutImplementations) :-
  seen(Module, Name, Arity),
  my_member(Module, Options),
  functor(Functor, Name, Arity),
  findall((my_clause(Module, Functor, Body), Positions), has_cut(my_clause(Module, Functor, Body), Positions), CutImplementations),
  CutImplementations \= [],
  findall(my_clause(Module, Functor, Body), clause(Module:Functor, Body), AllImplementations).

assert_clause(my_clause(Module, Functor, Body)) :-
  assertz(Module:(Functor :- Body)).

retractall_clause(my_clause(Module, Functor, _Body)) :-
  Functor =.. [Head|Tail],
  general_tail(Tail, NewTail),
  AlternativeFunctor =.. [Head|NewTail],
  retractall(Module:AlternativeFunctor).

general_tail([], []) :- !.
general_tail([_|Tail], [_|NewTail]) :-
  general_tail(Tail, NewTail).

check_result('_', _) :- !.
check_result(Input, Input).

% Beware: SICStus measures time in milliseconds, SWI in seconds.
calc_cap(AddConstant,Start, End, Cap) :-
  Time is End - Start,
  CapMs is Time / 1000,
  Cap is (ceiling(CapMs) * 2) + AddConstant.

set_my_stream :-
  open('/dev/null', write, Stream),
  set_error(Stream).

set_my_stream(Error, Stream) :-
  current_error(Error),
  open('/dev/null', write, Stream),
  set_error(Stream).

current_error(Stream) :-
    stream_property(Stream, alias(user_error)), !.

set_error(Stream) :-
    set_stream(Stream, alias(user_error)).

reset_my_stream(Error, Stream) :-
  set_error(Error),
  close(Stream).

my_run([], Tests-_) :- !,
  findall(my_test(Module, Block, Name), my_test(Module, Block, Name), Tests).
my_run(Options, Tests) :-
  find_tests(Options, [], Tests).

my_run_tests([]-[]) :- !.
my_run_tests([my_test(_, Block, Name)|Tests]-A) :-
  plunit:run_tests(Block:Name),
  my_run_tests(Tests-A).

find_tests([], Tests-A, Tests-A).
find_tests([Module|Modules], CurrentTests, Result) :-
  findall(my_test(Module, Block, Name), my_test(Module, Block, Name), Tests),
  (CurrentTests = [] -> Tests-_ = NewTests; my_append(CurrentTests, Tests-_, NewTests)),
  find_tests(Modules, NewTests, Result).

my_append(Element-List, List-Diff, Element-Diff).

my_member(Element, [Head|_]) :-
  Element == Head, !.
my_member(Element, [_|Tail]) :-
  my_member(Element, Tail).

check_for_flag(Left, flag) :-
  atom(Left) , !.
check_for_flag(Left, flag) :-
  var(Left) , !.
check_for_flag(Left, no_flag).

cleanup :-
  retractall(mutation_result(_, _, _, _, _)),
  retractall(has_cut(_, _)),
  retractall(redefined_instance(_, cut_supporter, _, _)).

swi_exception_handler(Exception) :-
  Exception == time_limit_exceeded,
  asserta(infinite_loop),
  fail.

print_result(DeadCount, LivingCount, LoopCount, AllMutations, 0) :- !,
  Mutationscore is 0,
  format('~w generated mutants~n~n', [AllMutations]),
  format('~w living mutants~n', [LivingCount]),
  format('~w dead mutants~n', [DeadCount]),
  format('~w timeouts~n~n', [LoopCount]), 
  format('Mutation score: ~1f%~n~n', [Mutationscore]).

print_result(DeadCount, LivingCount, LoopCount, AllMutations, Mutations) :-
  Score is DeadCount / Mutations,
  Mutationscore is Score * 100,
  format('~w generated mutants~n~n', [AllMutations]),
  format('~w living mutants~n', [LivingCount]),
  format('~w dead mutants~n', [DeadCount]),
  format('~w timeouts~n~n', [LoopCount]), 
  format('Mutation score: ~1f%~n~n', [Mutationscore]).

export(LivingMutants, DeadMutants, LoopMutants, LivingCount, DeadCount, LoopCount, AllMutations, Mutations) :-
  Score is DeadCount / Mutations,
  Mutationscore is Score * 100,
  current_output(Out),
  open('export.txt', write, Stream),
  set_output(Stream),
  format('~nMUTATION SCORE: ~1f%~n~n~n', [Mutationscore]),
  format('~w generated mutants~n~n', [AllMutations]),
  format('~w living mutants~n', [LivingCount]),
  format('~w dead mutants~n', [DeadCount]),
  format('~w timeouts~n~n~nANALYSIS:~n~n~n', [LoopCount]),
  format('LIVING MUTANTS: ~w~n~n~n', [LivingCount]),
  export_printer(LivingMutants),
  format('~n~nDEAD MUTANTS: ~w~n~n~n', [DeadCount]),
  export_printer(DeadMutants),
  format('~n~nTIMEOUT: ~w~n~n~n', [LoopCount]),
  export_printer(LoopMutants),
  set_output(Out),
  close(Stream).

export_printer([]).
export_printer([Head|Tail]) :-
  Head = mutation_result(Predicate, Mutation, Source, Mutant, _),
  Source = [my_clause(Module, _, _)|_],
  format('Module: ~w ~nPredicate: ~w ~nMutation: ~w ~nSource:~n~n', [Module, Predicate, Mutation]),
  export_printer_aux(Source),
  format('~nMutant: ~n~n', []),
  export_printer_aux(Mutant),
  format('~n$~n~n', []),
  export_printer(Tail).

export_printer_aux([]).
export_printer_aux([Clause|Tail]) :-
  Clause = my_clause(_, Head, Body),
  format('~w :- ~w~n', [Head, Body]),
  export_printer_aux(Tail).

find_mutations(basic, Mutations) :- !,
  findall(Mutation, basic(Mutation), Mutations).

find_mutations(deep, Mutations) :- !,
  findall(Mutation, deep(Mutation), Mutations).

find_mutations(experimental, Mutations) :- !,
  findall(Mutation, experimental(Mutation), Mutations).

find_mutations(Changes, Mutations) :-
  (
    memberchk(permute_cut(_), Changes) ->
      (
        \+ memberchk(mutate_arity0(_, _), Changes) ->
          asserta((redefined_instance(_, cut_supporter, _, _) :- fail)),
          Mutations = [mutate_arity0(_, cut_supporter)|Changes]
          ;
          true
      )
      ;
      Changes = Mutations
  ).

prepare_result(KindOfChange, (Name/Arity), Mutation) :-
  functor(KindOfChange, Change, _),
  KindOfChange =.. Changes,
  (
    Change == mutate_arity0 ->
      Changes = [_, Predicate, ActualMutation],
      Predicate =.. [Name|Arguments],
      length(Arguments, Arity),
      Mutation = ActualMutation;
      Changes = [Mutation, Predicate],
      Predicate =.. [Name|Arguments],
      length(Arguments, Arity)
  ).

% MUTATION EXECUTERS
mutate_functor([Head|Tail], Functor, Redefinition, [NewHead|Tail]) :-
  Head = my_clause(Module, Fun, Body),
  Body =.. List,
  redefine_functor(List, Functor, Redefinition, Result),
  NewBody =.. Result,
  NewHead = my_clause(Module, Fun, NewBody).

mutate_functor([Head|Tail], Functor, Redefinition, [Head|NewTail]) :-
  mutate_functor(Tail, Functor, Redefinition, NewTail).

redefine_functor([Functor|Tail], Functor, Redefinition, [Redefinition|Tail]).
redefine_functor([Head|Tail], Functor, Redefinition, [Head|NewTail]) :-
  Tail \= [],
  redefining_functor(Tail, Functor, Redefinition, NewTail).

redefining_functor([Head|Tail], Functor, Redefinition, [NewHead|Tail]) :-
  nonvar(Head),
  Head =.. List,
  redefine_functor(List, Functor, Redefinition, Result),
  NewHead =.. Result.

redefining_functor([Head|Tail], Functor, Redefinition, [Head|NewTail]) :-
  redefining_functor(Tail, Functor, Redefinition, NewTail).

% arity0
mutate_instances([Head|Tail], KindOfChange, [NewHead|Tail]) :-
  Head = my_clause(Module, Functor, Body),
  Functor =.. List,
  redefine_instance(List, KindOfChange, 0, Head, Result),
  NewFunctor =.. Result,
  NewHead = my_clause(Module, NewFunctor, Body).

mutate_instances([Head|Tail], KindOfChange, [NewHead|Tail]) :-
  Head = my_clause(Module, Functor, Body),
  Body =.. List,
  List \= [true],
  List \= [false],
  List \= [fail],
  redefine_instance(List, KindOfChange, 0, Head, Result),
  NewBody =.. Result,
  NewHead = my_clause(Module, Functor, NewBody).

mutate_instances([Head|Tail], KindOfChange, [Head|NewTail]) :-
  mutate_instances(Tail, KindOfChange, NewTail).

redefine_instance([Head], KindOfChange, _, Clause, [NewHead]) :-
  redefined_instance(Head, KindOfChange, Clause, NewHead).

redefine_instance([Head], _, Position, Clause, _) :-
  atom(Head),
  Head = !,
  \+ has_cut(Clause, _),
  !,
  assertz(has_cut(Clause, [Position|A]-A)),
  fail.

redefine_instance([Head], _, Position, Clause, _) :-
  atom(Head),
  Head = !,
  has_cut(Clause, List),
  my_append(List, [Position|A]-A, Result),
  retract(has_cut(Clause, List)),
  assertz(has_cut(Clause, Result)),
  fail.

redefine_instance([Head|Tail], KindOfChange, Position, Clause, [Head|NewTail]) :-
  (Head = ','; Head = ';'),
  !,
  NextPosition is Position + 1,
  redefining_instance(Tail, KindOfChange, NextPosition, Clause, NewTail).

redefine_instance([Head|Tail], KindOfChange, Position, Clause, [Head|NewTail]) :-
  redefining_instance(Tail, KindOfChange, Position, Clause, NewTail).

redefining_instance([Head|Tail], KindOfChange, Position, Clause, [NewHead|Tail]) :-
  var(Head),
  redefine_instance([Head], KindOfChange, Position, Clause, Result),
  PotentialHead =.. Result, % if var(Result) = true -> =.. is exploding
  check_result(PotentialHead, NewHead).

redefining_instance([Head|Tail], KindOfChange, Position, Clause, [NewHead|Tail]) :-
  nonvar(Head),
  Head =.. List,
  redefine_instance(List, KindOfChange, Position, Clause, Result),
  PotentialHead =.. Result,
  check_result(PotentialHead, NewHead).

redefining_instance([Head|Tail], KindOfChange, Position, Clause, [Head|NewTail]) :-
  redefining_instance(Tail, KindOfChange, Position, Clause, NewTail).

:- if(current_prolog_flag(dialect, swi)).

redefined_instance(Instance, variable_to_anonymous, Clause, '_') :-
  var(Instance),
  Clause = my_clause(_, Functor, Body),
  term_singletons((Functor:- Body), Singletons),
  (my_member(Instance, Singletons) -> fail; true).

:- elif(current_prolog_flag(dialect, sicstus)).

redefined_instance(Instance, variable_to_anonymous, Clause, '_') :-
  var(Instance),
  Clause = my_clause(_, Functor, Body),
  occurrences_of_var(Instance, (Functor:- Body), Count),
  (Count == 1 -> fail; true).

:- endif.

redefined_instance(Instance, true_to_false, _, false) :-
  atom(Instance),
  Instance = true.

redefined_instance(Instance, false_to_true, _, true) :-
  atom(Instance),
  (Instance = false; Instance = fail).

redefined_instance(Instance, atom_to_anonymus, Clause, '_') :-
  atom(Instance),
  Instance \= true,
  Instance \= false,
  Instance \= !,
  Instance \= repeat,
  Instance \= fail,
  Instance \= [],
  Clause = my_clause(Module, _, _),
  catch(call(Module:Instance), _, Result = failed),
  Result == failed.

redefined_instance(Instance, increase_number, _, Value) :-
  number(Instance),
  Value is Instance + 1.

redefined_instance(Instance, decrease_number, _, Value) :-
  number(Instance),
  Value is Instance - 1.

redefined_instance(Instance, empty_list_to_anonymous, _, '_') :-
  atomic(Instance),
  Instance = [].

% cuts
permute_cut([Head|Tail], [(CutHead, Positions)|CutTail], [Head|NewTail]) :-
  Head \= CutHead,
  !,
  permute_cut(Tail, [(CutHead, Positions)|CutTail], NewTail).

permute_cut([Head|Tail], [(Head, Positions)|_], [NewHead|Tail]) :-
  Head = my_clause(Module, Functor, Body),
  Body \= !,
  permuting_cut(Body, 1, Positions, NewBody),
  NewHead = my_clause(Module, Functor, NewBody).

permute_cut([Head|Tail], [(Head, _)|CutTail], [Head|NewTail]) :-
  permute_cut(Tail, CutTail, NewTail).

permuting_cut(_, _, []-[], _) :- !, fail.
permuting_cut(','(Left, Right), Counter, [Position|_]-_, NewBody) :-
  Pos is Position - 1,
  Pos == Counter,
  remove_cut(Left, Right, EdgeCase, NewBody),
  (nonvar(EdgeCase) -> !; true).
permuting_cut(','(Left, Right), Position, [Position|_]-_, ','(!, Left)) :-
  atom(Right),
  Right = !,
  !.
permuting_cut(','(Left, Right), Counter, [Position|Positions]-A, ','(Left, NewRight)) :-
  NewCounter is Counter + 1,
  (Counter == Position -> NewPositions = Positions-A; NewPositions = [Position|Positions]-A),
  permuting_cut(Right, NewCounter, NewPositions, NewRight).

add_cut(','(Left, Right), ','(Left, NewRight)) :-
  NewRight == ','(!, Right), !.
add_cut(Right, ','(Right,!)).

% standard case
remove_cut(OldLeft, ','(Left, Right), _NoEdgeCase, ','(NewLeft, Right)) :-
  atom(Left),
  Left = !,
  !,
  NewLeft = ','(!,OldLeft).
remove_cut(OldLeft, ','(Left, _Right), edge_case, ','(OldLeft, ','(!, Left))).

negate_expression([Head|Tail], [NewHead|Tail]) :-
  Head = my_clause(Module, Functor, Body),
  Body \= true,
  Body \= false,
  Body \= fail,
  negating_expression(Body, NewBody),
  NewHead = my_clause(Module, Functor, NewBody).
negate_expression([Head|Tail], [Head|NewTail]) :-
  negate_expression(Tail, NewTail).

negating_expression(','(Left, Right), ','(\+(Left), Right)) :- !.
negating_expression(';'(Left, Right), ';'(NewLeft, Right)) :-
  !,
  negating_expression(Left, NewLeft).
negating_expression(Leaf, \+(Leaf)).

% MUTATIONS
mutate_pred(Functor, conjunction_to_disjunction(Functor), (','), (';')).
mutate_pred(Functor, disjunction_to_conjunction(Functor), (';'), (',')).
mutate_pred(Functor, unif_equal_to_unif_nonequal(Functor), (=), (\=)).
mutate_pred(Functor, unif_nonequal_to_unif_equal(Functor), (\=), (=)).
mutate_pred(Functor, arith_equal_to_arith_nonequal(Functor), (=:=), (=\=)).
mutate_pred(Functor, arith_nonequal_to_arith_equal(Functor), (=\=), (=:=)).
mutate_pred(Functor, equal_to_nonequal(Functor), (==), (\==)).
mutate_pred(Functor, nonequal_to_equal(Functor), (\==), (==)).
mutate_pred(Functor, add_to_sub(Functor), (+), (-)).
mutate_pred(Functor, sub_to_add(Functor), (-), (+)).
mutate_pred(Functor, mul_to_add(Functor), (*), (+)).
mutate_pred(Functor, div_to_sub(Functor), (/), (-)).
mutate_pred(Functor, negate_gt(Functor), (>), (=<)).
mutate_pred(Functor, negate_lt(Functor), (<), (>=)).
mutate_pred(Functor, negate_ge(Functor), (>=), (<)).
mutate_pred(Functor, negate_le(Functor), (=<), (>)).
mutate_pred(Functor, edge_gt(Functor), (>), (==)).
mutate_pred(Functor, edge_lt(Functor), (<), (==)).
mutate_pred(Functor, edge_ge(Functor), (>=), (==)).
mutate_pred(Functor, edge_le(Functor), (=<), (==)).

mutate(remove_all(Functor), Options, Implementations, [], MutationOrFix) :-
  pred_iterator(Options, Functor, Implementations),
  mutate_aux(Implementations, [], MutationOrFix).
mutate(reverse_predicate(Functor), Options, Implementations, NewImplementations, MutationOrFix) :-
  pred_iterator(Options, Functor, Implementations),
  reverse(Implementations, NewImplementations),
  mutate_aux(Implementations, NewImplementations, MutationOrFix).
mutate(MutationCommand, Options, Implementations, NewImplementations, MutationOrFix) :-
  pred_iterator(Options, Functor, Implementations),
  mutate_pred(Functor, MutationCommand, Before, After),
  mutate_functor(Implementations, Before, After, NewImplementations),
  mutate_aux(Implementations, NewImplementations, MutationOrFix).
mutate(mutate_arity0(Functor, KindOfChange), Options, Implementations, NewImplementations, MutationOrFix) :-
  pred_iterator(Options, Functor, Implementations),
  mutate_instances(Implementations, KindOfChange, NewImplementations),
  mutate_aux(Implementations, NewImplementations, MutationOrFix).
mutate(negate_expression(Functor), Options, Implementations, NewImplementations, MutationOrFix) :-
  pred_iterator(Options, Functor, Implementations),
  negate_expression(Implementations, NewImplementations),
  mutate_aux(Implementations, NewImplementations, MutationOrFix).
mutate(permute_cut(Functor), Options, AllImplementations, NewImplementations, MutationOrFix) :-
  pred_iterator(Options, Functor, AllImplementations, CutImplementations),
  permute_cut(AllImplementations, CutImplementations, NewImplementations),
  mutate_aux(AllImplementations, NewImplementations, MutationOrFix).

% SET UP PREDICATES
add_dynamic(plunit, _, _) :- !, fail.
add_dynamic(_Module, (test(_) :- _), _) :- !, fail.
add_dynamic(_Module, (test(_, _) :- _), _) :- !, fail.
add_dynamic(_Module, end_of_file, _) :- !, fail.
add_dynamic(_Module, (:- _), _) :- !, fail.
add_dynamic(_Module, (_-->_), _) :- !, fail.

add_dynamic(Module, (Head :- _), (:- dynamic Name/Arity)) :- !,
  functor(Head, Name, Arity),
  \+ seen(Module, Name, Arity),
  assertz(seen(Module, Name,Arity)).

add_dynamic(Module, Head, (:- dynamic Name/Arity)) :- !,
  functor(Head, Name, Arity),
  \+ seen(Module, Name, Arity),
  assertz(seen(Module, Name, Arity)).

collect_stuff(_, plunit) :- !.
collect_stuff(end_of_file, _) :- !.
collect_stuff((_-->_), _) :- !.
collect_stuff((:- begin_tests(Block)), Module) :-
  asserta(my_current_module(Module)),
  asserta(current_tests(Block)), !.
collect_stuff((:- end_tests(Block)), Module) :-
  retract(my_current_module(Module)),
  retract(current_tests(Block)), !.
collect_stuff((:- _), _) :- !.
collect_stuff((test(Name) :- _), _) :-
  my_current_module(Module),
  current_tests(Block),
  assertz(my_test(Module, Block, Name)), !.
collect_stuff((test(Name, _) :- _), _) :-
  my_current_module(Module),
  current_tests(Block),
  assertz(my_test(Module, Block, Name)), !.
collect_stuff((Functor :- Body), Module) :-
  assertz(my_clause(Module, Functor, Body)), !.
collect_stuff(Functor, Module) :-
  assertz(my_clause(Module, Functor, true)).

:- if(current_prolog_flag(dialect, swi)).

% enable term expansion for SWI
:- multifile term_expansion/4.
user:term_expansion(TermIn, LayoutIn, TermsOut, LayoutOut) :-
    prolog_load_context(module, Module),
    LayoutOut = LayoutIn,
    collect_stuff(TermIn, Module),
    (
      add_dynamic(Module, TermIn, Dynamic) ->
        TermsOut = [Dynamic, TermIn]
        ;
        TermsOut = TermIn
    ).

:- elif(current_prolog_flag(dialect, sicstus)).

user:portray_message(_Severity, plunit(failed(FailedTests))) :-
     FailedTests > 0,
     asserta(test_failed), !.

term_expansion_aux(TermIn, Module, TermOut) :-
    collect_stuff(TermIn, Module),
    add_dynamic(Module, TermIn, Dynamic),
    !,
    TermOut = [Dynamic, TermIn].
term_expansion_aux(TermIn, _, TermIn).

% enable term expansion for SICStus
:- multifile user:term_expansion/6.
user:term_expansion(TermIn, LayoutIn, Ids, TermOut, LayoutIn, [mutation_testing_token|Ids]) :-
    nonmember(mutation_testing_token, Ids),
    prolog_load_context(module, Module),
    term_expansion_aux(TermIn, Module, TermOut).

:- endif.
