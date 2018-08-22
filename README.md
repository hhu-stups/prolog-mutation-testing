## Mutation Testing Framework for SWI and SICStus Prolog:

# How to use (for SICStus replace swipl with sicstus -l):


$ swipl mutation_testing.pl // load the mutation tester 

?- [your_file]. // load your file(s) or mudule(s)

?- mutation_test. // launch the mutation testing process


OR


:- use_module(mutation_testing). // type it at the very top 
                                    (directly after :- module(yourmodule, [args]).)
                                    of your module under test
                                    
$ swipl yourmodule.pl 

?- mutation_testing:mutation_test. 


# Interface:

Provided predicates within the framework are mutation_test/0, mutation_test/1 and mutation_test/2.

  mutation_test(+Modules, +Depth).

    Modules is a list of modules under test. By using the empty list [] all loaded modules, except for internal Prolog
    libraries, will be mutated and tested.
    Depth is either an atom (basic, deep, experimental) which dictates the amount and kind of mutations or a list with
    explicit mutations.
    
  mutation_test(+Input).
  
    If input is not a list, mutation_test([], Input) is being called. Otherwise, mutation_test(Input, basic) is being called.
    
  mutation_test.
  
    Calls mutation_test([], basic).
