module Main where

import Compile
import Text.Printf
import IR
import Estimate

import IRPhase2VirtRegs
import IRUPhaseDCE
import IRPhaseSO
import IRPhaseSEO

name = "AdventOfCode : 2021 : 24 - simpliest IR compiler"
major_version = 0 :: Int
minor_version = 5 :: Int

input_file = "example_1/input.txt"
output_prefix = "example_1/out"

global_uses = ["globaluse z"]

input_vect = [1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5]
actualPhases = [iRPhase2VirtRegs, iRPhaseSO, iRPhaseSEO]

main :: IO ()
main = do
    text <- readFile input_file
    let irs1 = compilerCompile actualPhases global_uses text
    let irs = compilerApplyExec input_vect irs1
    compilerSavePhases output_prefix irs

    putStrLn $ printf "%s. Version: %d.%d" name major_version minor_version
    putStrLn $ printf "\tinput file = \"%s\"" input_file
    putStrLn $ printf "\toutput files prefix = \"%s...\"" output_prefix
    putStrLn "... [OK]"
