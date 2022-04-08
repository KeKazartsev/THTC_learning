module Main where

import Compile
import Text.Printf
import IRPhase2VirtRegs
import IRPhaseDCE

name = "AdventOfCode : 2021 : 24 - simpliest IR compiler"
major_version = 0 :: Int
minor_version = 2 :: Int

input_file = "example_1/input.txt"
output_prefix = "example_1/out"

global_uses = ["globaluse z"]

main :: IO ()
main = do
    text <- readFile input_file
    let irs = compilerCompile [iRPhase2VirtRegs, iRPhaseDCE] global_uses text
    compilerSavePhases output_prefix irs

    putStrLn $ printf "%s. Version: %d.%d" name major_version minor_version
    putStrLn $ printf "\tinput file = \"%s\"" input_file
    putStrLn $ printf "\toutput files prefix = \"%s...\"" output_prefix
    putStrLn "... [OK]"
