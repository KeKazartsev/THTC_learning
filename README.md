# d24

Study project on haskell.
Solving task: https://adventofcode.com/2021/day/24

I. Destination:
Haskell studying project: small IR-optimazing compiler (for a very simple "instructions set") for proper and general solving given task.
    1. To realize IR-optimizations, which have to eliminate redundant code: "a = 5 + b - b" will become "a = 5"
    2. After optimizations will be done - execute given program and show eventual machime state.

II. The task.
Task (adventof code, year 2021, task 24) after slight simplification is that:
    1. You have program (approximately 250 instractions) with input vector - 14 digits.
    2. Find the biggest vector in alphabetic order, at which program output sutisfies pre-given condition.
So total amount of computation approximately is: 10^14 * 250 * "program speed ~ 100 tacts / instructions" = 2^61 ticks.
This is much bigger, than we could execute on personal computer.

III. Bypass solution.
We also represent ungeneral, optimistic and ungaranteed solution (in separate program):
    1. guess (through subsitution only "0" and "9" into every vector index - "2^14 * 250 * speed" bruteforce) which from 14 input numbers is affect the result (i.e. "useful") and which is not  (i.e. "redundant")
    2. set up redundant nubers to constant literal "9" (they supposetly have no impact in output)
    3. burte force useful numbers
So in this way we'll receive optimistic answer.

