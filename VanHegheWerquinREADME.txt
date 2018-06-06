APLAI Project 17-18
-------------------

The solution to the first part can be found under the sudoku directory.
The eclipse implmentation in the eclipse directory and the chr implementation under the chr directory.

The solution to the second part can be found in the hitori directory,
the same eclipse and chr directories are found here.

Sudoku
------

The code for the original viewpoint in eclipse can be found in sudoku/eclipse/original_viewpoint.pl.
The goal 'solutions' solves all puzzles and reports their time.
The new viewpiont is found in sudoku/eclipse/new_viewpoint.pl.
The goal 'solutions_new' solves all puzzles using the new viewpoint.
Channeling can be found in sudoku/eclipse/channeling.pl,
with the goal 'solutions_chan' running all puzzles.

The chr implementation has the same stucture under the sudoku/chr directory.
The goals have the same names: 'solutions', 'solutions_new', and 'solutions_chan'.

Hitori
------

The eclipse solver for the hitori puzzles is found in hitori/eclipse/solver.pl.
The goal 'solutions' solves all puzzles and reports their times and other statistics.

The chr solvers are under hitori/chr/solver.pl, for the solver that implements the connectivity constraint
as a passive constraint. The active solver is in hitori/chr/solver_active.pl.
The same goal solves all puzzles: 'solutions'.