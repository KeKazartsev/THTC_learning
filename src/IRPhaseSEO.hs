{-
 - Phase to simplify using every single oper. Providing it's estimation.
 --}
module IRPhaseSEO (
  iRPhaseSEO,
) where

import Reg
import Oper
import IR
import Control.Lens
import Calculate
import Estimate
import qualified Data.Map.Strict as M

iRPhaseSEO :: IR -> IR
iRPhaseSEO ir = if _nPhase ir /= IRPhCreate
    then (nPhase .~ IRPhSEO) . (iOpers .~ iopers') $ ir
    else error $ "Simplify Estimated Opers phase (IRPhSEO) can't be run on physical regs. Run IR phase 2VirtRegs before"
    where
    iopers' = doSO ir

doSO :: IR -> [IOper]
doSO ir = ios2
    where
    reg2est = M.compose (_ests ir) (_vIOpers ir)
    ios = _iOpers ir
    ios1 = map (\(i, o) -> (i, substitutePrecise reg2est o)) ios
    ios2 = ios1 --map (simplifyEstOper reg2est) ios

substitutePrecise :: M.Map Var EstimateRes -> Oper -> Oper
substitutePrecise reg2est o@(Oper op d u1 u2) = Oper op d u1' u2'
    where
    u1' = case u1 of
        RVar v -> case M.lookup v reg2est of
            Just (Just (x, y)) -> if x == y then Number x else u1
            otherwise -> u1
        otherwise -> u1
    u2' = case u2 of
        RVar v -> case M.lookup v reg2est of
            Just (Just (x, y)) -> if x == y then Number x else u2
            otherwise -> u2
        otherwise -> u2

simplifyEstOper :: M.Map Var EstimateRes -> Oper -> Oper
simpoifyEstOper (Oper op d (Number n1) (Number n2)) = Oper Set d (Number $ calcOper op n1 n2) None
simplifyEstOper reg2est o@(Oper op d u1 u2)
    | op == Eql && both && (mx1 < mn2 || mx2 < mn1) = Oper Set d (Number 0) None
    | otherwise = o
    where
    ((mn1, mx1), is1) = case u1 of
        Number x -> ((x, x), True)
        RVar v -> case M.lookup v reg2est of
            Just (Just (x, y)) -> ((x, y), True)
            otherwise -> ((0, 0), False)
        otherwise -> ((0, 0), False)
    ((mn2, mx2), is2) = case u2 of
        Number x -> ((x, x), True)
        RVar v -> case M.lookup v reg2est of
            Just (Just (x, y)) -> ((x, y), True)
            otherwise -> ((0, 0), False)
        otherwise -> ((0, 0), False)
    both = is1 && is2
