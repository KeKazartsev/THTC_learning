module Compile (
    compilerCompile,
    compilerSavePhases, compilerExecPhases,
    compilerLastPhase,
    compilerApplyExec,
) where

import Parse
import IR
import Reg
import Calculate
import Lib
import qualified Data.Map.Strict as M
import Control.Lens

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
compilerExecPhases irs input = map (compilerExecPhase input) irs

compilerExecPhase input ir = M.toList $ M.compose idx2Res arg2Idx
    where
    arg2Idx = mapSwapStrict $ _gUses ir
    idx2Res = execIR input ir

compilerApplyExec :: [Int] -> [IR] -> [IR]
compilerApplyExec input irs = map (applyExec input) irs
    where
    applyExec inp ir = (calcs .~ execIR inp ir) $ ir

compilerSavePhases :: String -> [IR] -> IO()
compilerSavePhases file_prefix irs = mapM_ (saveIR file_prefix) irs

compilerLastPhase :: [IR] -> IR
compilerLastPhase = last
