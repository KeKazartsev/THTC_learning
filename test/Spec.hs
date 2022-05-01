import Data.List.Extra
import Compile

import IRPhase2VirtRegs
import IRUPhaseDCE
import IRPhaseSO

import Test.QuickCheck

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
actualPhases = [iRPhase2VirtRegs, iRUPhaseDCE, iRPhaseSO, iRUPhaseDCE]

g_uses = ["globaluse z"]
phases = [iRPhase2VirtRegs, iRUPhaseDCE, iRPhaseSO, iRUPhaseDCE]
irs = compilerCompile phases g_uses program_text
doExec = compilerExecPhases irs

isOK :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Property
isOK a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 =
    (collect (bb !! 0)) $ allSame (doExec bb)
    where
    aa = [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14]
    bb = map (\x -> 1 + mod (abs x) 9) aa


main :: IO ()
main = do
    quickCheckWith stdArgs { maxSuccess = 1000 } isOK
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
