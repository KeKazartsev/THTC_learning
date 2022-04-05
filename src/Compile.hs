module Compile (
    compilerCompile,
    compilerSavePhases, compilerExecPhases,
    compilerLastPhase,
) where

import Parse
import IR
import Reg
import Calculate

compilerMakeIR :: [String] -> String -> IR
compilerMakeIR uses text = case (doParseIR uses text) of
    Left err -> error (show err)
    Right ir -> ir

compilerApplyPhases :: [IR -> IR] -> IR -> [IR]
compilerApplyPhases [] ir = [ir]
compilerApplyPhases (f:fs) ir = ir : compilerApplyPhases fs (f ir)

compilerCompile :: [IR -> IR] -> [String] -> String -> [IR]
compilerCompile phases uses text =
    compilerApplyPhases phases $ compilerMakeIR uses text

compilerExecPhases :: [IR] -> [Int] -> [[(Var, Int)]]
compilerExecPhases irs input = map (execIR input) irs

compilerSavePhases :: String -> [IR] -> IO()
compilerSavePhases file_prefix irs = mapM_ (saveIR file_prefix) irs

compilerLastPhase :: [IR] -> IR
compilerLastPhase = last
