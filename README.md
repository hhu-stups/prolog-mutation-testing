Mutation Tester for Prolog:

How to use:


$ swipl mutation_testing.pl // load the mutation tester

?- [your_file]. // load your file(s) or mudule(s)

?- mutation_test. // start the mutation tester


OR


:- use_module(mutation_testing). // type it at the very top 
                                    (directyl after :- module(yourmodule, [args]).)
                                    of your module that you want to test
                                    
$ swipl yourmodule.pl 

?- mutation_testing:mutation_test. 


Features:

You can alternatively use mutation_test/1 to specify your projects modules 
you want to test

?- mutation_test([module1, module2]).

OR

?- mutation_test([]). // equals: mutation_test.