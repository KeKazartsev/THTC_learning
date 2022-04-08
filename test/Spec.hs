import Data.List.Extra
import Compile
import IRPhase2VirtRegs
import IRPhaseDCE

{-
 - Checking IR compiler optimization correctness.
 - IR compiler optimize program step-by-step, changin it's operations,
 - but it has to save program semantic <=> for any possible program input program result have to be unchanged
 -
 - So the test is:
 -  1. Compile program phase-by-phase
 -  2. Generate some input
 -  3. Check: if program result the same for every compiled phase
 -}

input_vect = [1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5]

main :: IO ()
main = do
    let irs = compilerCompile [iRPhase2VirtRegs, iRPhaseDCE] ["globaluse z"] program_text
    let ress = compilerExecPhases irs input_vect
    putStrLn $ if allSame ress then "[OK]" else "[FAIL!]"
    return ()



program_text = "inp w\n\
\mul x 0\n\
\add x z\n\
\mod x 26\n\
\div z 1\n\
\add x 11\n\
\eql x w\n\
\eql x 0\n\
\mul y 0\n\
\add y 25\n\
\mul y x\n\
\add y 1\n\
\mul z y\n\
\mul y 0\n\
\add y w\n\
\add y 5\n\
\mul y x\n\
\add z y\n\
\inp w\n\
\mul x 0\n\
\add x z\n\
\mod x 26\n\
\div z 1\n\
\add x 13\n\
\eql x w\n\
\eql x 0\n\
\mul y 0\n\
\add y 25\n\
\mul y x\n\
\add y 1\n\
\mul z y\n\
\mul y 0\n\
\add y w\n\
\add y 5\n\
\mul y x\n\
\add z y\n\
\inp w\n\
\mul x 0\n\
\add x z\n\
\mod x 26\n\
\div z 1\n\
\add x 12\n\
\eql x w\n\
\eql x 0\n\
\mul y 0\n\
\add y 25\n\
\mul y x\n\
\add y 1\n\
\mul z y\n\
\mul y 0\n\
\add y w\n\
\add y 1\n\
\mul y x\n\
\add z y\n\
\inp w\n\
\mul x 0\n\
\add x z\n\
\mod x 26\n\
\div z 1\n\
\add x 15\n\
\eql x w\n\
\eql x 0\n\
\mul y 0\n\
\add y 25\n\
\mul y x\n\
\add y 1\n\
\mul z y\n\
\mul y 0\n\
\add y w\n\
\add y 15\n\
\mul y x\n\
\add z y\n\
\inp w\n\
\mul x 0\n\
\add x z\n\
\mod x 26\n\
\div z 1\n\
\add x 10\n\
\eql x w\n\
\eql x 0\n\
\mul y 0\n\
\add y 25\n\
\mul y x\n\
\add y 1\n\
\mul z y\n\
\mul y 0\n\
\add y w\n\
\add y 2\n\
\mul y x\n\
\add z y\n\
\inp w\n\
\mul x 0\n\
\add x z\n\
\mod x 26\n\
\div z 26\n\
\add x -1\n\
\eql x w\n\
\eql x 0\n\
\mul y 0\n\
\add y 25\n\
\mul y x\n\
\add y 1\n\
\mul z y\n\
\mul y 0\n\
\add y w\n\
\add y 2\n\
\mul y x\n\
\add z y\n\
\inp w\n\
\mul x 0\n\
\add x z\n\
\mod x 26\n\
\div z 1\n\
\add x 14\n\
\eql x w\n\
\eql x 0\n\
\mul y 0\n\
\add y 25\n\
\mul y x\n\
\add y 1\n\
\mul z y\n\
\mul y 0\n\
\add y w\n\
\add y 5\n\
\mul y x\n\
\add z y\n\
\inp w\n\
\mul x 0\n\
\add x z\n\
\mod x 26\n\
\div z 26\n\
\add x -8\n\
\eql x w\n\
\eql x 0\n\
\mul y 0\n\
\add y 25\n\
\mul y x\n\
\add y 1\n\
\mul z y\n\
\mul y 0\n\
\add y w\n\
\add y 8\n\
\mul y x\n\
\add z y\n\
\inp w\n\
\mul x 0\n\
\add x z\n\
\mod x 26\n\
\div z 26\n\
\add x -7\n\
\eql x w\n\
\eql x 0\n\
\mul y 0\n\
\add y 25\n\
\mul y x\n\
\add y 1\n\
\mul z y\n\
\mul y 0\n\
\add y w\n\
\add y 14\n\
\mul y x\n\
\add z y\n\
\inp w\n\
\mul x 0\n\
\add x z\n\
\mod x 26\n\
\div z 26\n\
\add x -8\n\
\eql x w\n\
\eql x 0\n\
\mul y 0\n\
\add y 25\n\
\mul y x\n\
\add y 1\n\
\mul z y\n\
\mul y 0\n\
\add y w\n\
\add y 12\n\
\mul y x\n\
\add z y\n\
\inp w\n\
\mul x 0\n\
\add x z\n\
\mod x 26\n\
\div z 1\n\
\add x 11\n\
\eql x w\n\
\eql x 0\n\
\mul y 0\n\
\add y 25\n\
\mul y x\n\
\add y 1\n\
\mul z y\n\
\mul y 0\n\
\add y w\n\
\add y 7\n\
\mul y x\n\
\add z y\n\
\inp w\n\
\mul x 0\n\
\add x z\n\
\mod x 26\n\
\div z 26\n\
\add x -2\n\
\eql x w\n\
\eql x 0\n\
\mul y 0\n\
\add y 25\n\
\mul y x\n\
\add y 1\n\
\mul z y\n\
\mul y 0\n\
\add y w\n\
\add y 14\n\
\mul y x\n\
\add z y\n\
\inp w\n\
\mul x 0\n\
\add x z\n\
\mod x 26\n\
\div z 26\n\
\add x -2\n\
\eql x w\n\
\eql x 0\n\
\mul y 0\n\
\add y 25\n\
\mul y x\n\
\add y 1\n\
\mul z y\n\
\mul y 0\n\
\add y w\n\
\add y 13\n\
\mul y x\n\
\add z y\n\
\inp w\n\
\mul x 0\n\
\add x z\n\
\mod x 26\n\
\div z 26\n\
\add x -13\n\
\eql x w\n\
\eql x 0\n\
\mul y 0\n\
\add y 25\n\
\mul y x\n\
\add y 1\n\
\mul z y\n\
\mul y 0\n\
\add y w\n\
\add y 6\n\
\mul y x\n\
\add z y"
