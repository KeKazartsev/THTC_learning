Verson 0.1:
Unfinished, unstable, not even MVP DSL-compiler
Create some main compiler notions:
    - Reg - Registers description
    - IR - intermediete representation format
    - Oper - possible operations
And both main and auxilary compiler actions:
    - Parse - parse IR from text file
    - Calculate - calculate result (regs, mentioned in GlobalUse) of a program execution
    - Optimizations phases - empty plug
    - Save Phase to fule - for Debugging, view possible optimizations
    - Test - check if result of a program does not chaned through program optimizations
# Changelog for d24

## Unreleased changes
