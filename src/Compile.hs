module Compile (
    compilerCompile,
    compilerSavePhases, compilerExecPhases,
    --compilerLastPhase,
    compilerApplyExec,
) where

import Parse
import IR
import Reg
import Calculate
import Lib
import qualified Data.Map.Strict as M
import Control.Lens
import IRUPhaseDCE

compilerMakeIR :: [String] -> String -> IR
compilerMakeIR uses text = case (doParseIR uses text) of
    Left err -> error (show err)
    Right ir -> ir

compilerApplyPhases :: [IR -> IR] -> IR -> [IR]
compilerApplyPhases [] ir = []
compilerApplyPhases (f:fs) ir = comp ++ compilerApplyPhases fs (last comp)
    where comp = compilerApplyComplex f ir

compilerApplyComplex :: (IR -> IR) -> IR -> [IR]
compilerApplyComplex f ir = [ir1, ir2]
    where
    ir1 = compilerApplyMain f ir
    ir2 = compilerApplyUtility iRUPhaseDCE ir1

compilerApplyMain f ir = ir2
    where
    ir1 = f ir
    ir2 = iRCalcUtilStat $ (nPhaseNum +~ 1) . (nSubPhase .~ IRSubPhPhase) . (nSubPhaseNum .~ 0) $ ir1

compilerApplyUtility fu ir = ir2
    where
    ir1 = fu ir
    ir2 = iRCalcUtilStat $ (nSubPhaseNum +~ 1) $ ir1

compilerCompile :: [IR -> IR] -> [String] -> String -> [IR]
compilerCompile phases uses text = ir : compilerApplyPhases phases ir
    where ir = compilerMakeIR uses text

-- ============== EXEC

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

--compilerLastPhase :: [IR] -> IR
--compilerLastPhase = last
