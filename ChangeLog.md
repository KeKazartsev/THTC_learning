Version 0.3:
Add estimate technique:
    estimate result of every oper as (including) range: [Min_Value..Max_Value], accordingly to previously estimated arguments
    Some special cases:
        - Value can not exist (Nothing): for example in case of division by zero. In this case we use Maybe aggregate.
        - Value can be recovered from Nothing (not exist), for example "Eql" instruction always returns sub-range from [0..1].
        - Example:
            - v100 <- v99 Div v98 | v99 = [1..2], v98 = [0..3] => v100 = Nothing
            - v101 <- v100 Eql 15 | v100 = Nothing => V101 = [0..1]
    some corresponding auxiliary changes:
        - generalize text dump representation, in case different phases have to show difeerent information
        - generalize auxiliary Phase actions (calculate some strictly-requiring values)



Version 0.2:
Add phases:
     - Phys2VirtRegs - use virtual regs instead of physical
     - DCE - dead code eliminations
    Some auxiliary actions due to support this changes.



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
