module IRPhase2VirtRegs (
    iRPhase2VirtRegs,
) where

import Reg
import Oper
import IR
import Control.Monad.State
import qualified Data.Map.Strict as M
import Control.Lens

iRPhase2VirtRegs :: IR -> IR
iRPhase2VirtRegs ir = if _nPhase ir == IRPhCreate
    then (nPhase .~ IRPh2VirtRegs) $ iRPhase2VirtRegs' ir
    else error $ "IRphase2VirtRegs function must be calld directly after IRPhCreate; But curent Phase == " ++ show (_nPhase ir)

iRPhase2VirtRegs' :: IR -> IR
iRPhase2VirtRegs' ir = (iOpers .~ iopers') . (iRcnt .~ cnts') $ ir
    where
    (iopers', (vrn', _, _)) = runState (moveOpersToVirt (_iOpers ir)) (0, M.empty, M.empty)
    cnts' = (vrN .~ vrn') $ (_iRcnt ir) :: IRCnt

moveOpersToVirt :: [IOper] -> State (Int, M.Map Var Var, M.Map Var Var) [IOper]
moveOpersToVirt [] = do
  return []
moveOpersToVirt ((i, Oper op d u1 u2):os) = do
    (vrn, mp, regs) <- get
    let u2' = if isValRegGlobal u2
        then RVar $ getRegStrict regs (getValRegStrict u2)
        else u2
    let u1' = if isValRegGlobal u1
        then RVar $ getRegStrict regs (getValRegStrict u1)
        else u1
    d' <- if isRegGlobal d && op /= GlobalUse then do
        let vrn1 = vrn + 1
        let d1 = virtReg vrn1
        let regs1 = setReg regs d d1
        let mp1 = M.insert d1 d mp
        put (vrn1, mp1, regs1)
        return d1
    else return d
    rest <- moveOpersToVirt os
    return $ (i, Oper op d' u1' u2'):rest
