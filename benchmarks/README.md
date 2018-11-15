# mutation-testing-benchmarks

## Benchmarks

### alloy2b_standalone.pl

A partial translator from the formalism Alloy to the formal language B.
See also:
Sebastian Krings, Joshua Schmidt, Carola Brings, Marc Frappier, Michael Leuschel
A Translation from Alloy to B.
In Proceedings ABZ 2018, LNCS, 10817, Springer, 71--86, 2018.

### fifteen_puzzle_solver.pl

A solver for the 15-puzzle.

### ACOL VM

Usually, bytecode interpreters are far more efficient than AST interpreters.
In order to evaluate the performance different interpreter designs (AST, Bytecode, ...) in Prolog,
we designed a small imperative language named ACOL.
It only contains basic integer arithmetic, variable assignments, if-statements and while-loops.
Surprisingly, in Prolog, AST interpreters usually are more efficient than bytecode interpreters.

See also: Evaluating Interpreter Design in Prolog.
In 18. Kolloquium Programmiersprachen und Grundlagen der Programmierung KPS 2015,
Schriftenreihe des Instituts für Computersprachen, 2015.
Available at: http://www.complang.tuwien.ac.at/kps2015/proceedings/KPS_2015_submission_24.pdf

#### parser.pl

A basic parser for ACOL using DCGS.

#### ast_interpreter.pl

A basic AST interpreter for ACOL as it is idiomatic to write.

#### rational_trees.pl

A twist on the AST interpreter:
for while-loops, rational trees are used in order to (infinitely) expand all iterations of the loop.

#### compiler.pl

This module compiles the AST into a list of bytecodes.
The bytecode itself is designed for ACOL and not an existing one.

#### assert_bc.pl

A bytecode interpreter.
All instructions and arguments are asserted as facts for fast lookup instead of maintaining a list.
This allows to manage jumps in the code in a faily performant manner.

#### rt_bytecode.pl

A bytecode interpreter with a twist:
the bytecode is threaded in a manner where instead of jumps, again rational trees are used.
