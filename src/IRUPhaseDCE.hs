module IRUPhaseDCE (
  iRUPhaseDCE,
) where

import Reg
import Oper
import IR
import qualified Data.Set as S
import Control.Lens

iRUPhaseDCE :: IR -> IR
iRUPhaseDCE ir = if _nPhase ir /= IRPhCreate
    then (nSubPhase .~ IRSubPhDCE) . (iOpers .~ iopers') $ ir
    else error $ "Dead Code Elimination phase (IRUPhDCE) can't be run on physical regs. Run IR phase 2VirtRegs before"
    where
    iopers' = doDCE (_iOpers ir)

doDCE ops = filter (\(i, o) -> S.member (operDef o) live_opers) ops
    where live_opers = collectLO (reverse ops) S.empty

collectLO [] lo = lo
collectLO ((i, o@(Oper op d u1 u2)):os) lo = collectLO os lo2
    where
    isGoodOper = op == GlobalUse || S.member d lo
    lo0 = if isGoodOper then S.insert d lo else lo
    lo1 = if isGoodOper && isValReg u1
        then S.insert (getValRegStrict u1) lo0 else lo0
    lo2 = if isGoodOper && isValReg u2
        then S.insert (getValRegStrict u2) lo1 else lo1

