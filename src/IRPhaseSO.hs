{-
 - Phase to simplify using every single oper.
 - "Single oper" means we simplify only 1 operation, without "peephole" - locally look around the oper.
 --}
module IRPhaseSO (
  iRPhaseSO,
) where

import Reg
import Oper
import IR
import Control.Lens
import Calculate

iRPhaseSO :: IR -> IR
iRPhaseSO ir = if _nPhase ir /= IRPhCreate
    then (nPhase .~ IRPhSO) . (iOpers .~ iopers') $ ir
    else error $ "Simplify Opers phase (IRPhSO) can't be run on physical regs. Run IR phase 2VirtRegs before"
    where
    iopers' = doSO (_iOpers ir)

doSO :: [IOper] -> [IOper]
doSO ios = map (\(i, o) -> (i, simplifyOper o)) ios

simplifyOper (Oper op d (Number n1) (Number n2)) = Oper Set d (Number $ calcOper op n1 n2) None
simplifyOper o@(Oper op d arg1 arg2)
    | op == Add && arg2 == Number 0 = Oper Set d arg1 None
    | op == Mul && arg2 == Number 0 = Oper Set d (Number 0) None
    | op == Mul && arg2 == Number 1 = Oper Set d arg1 None
    | op == Div && (arg2 == Number 0) = undefined
    | op == Div && (arg1 == Number 0) = Oper Set d (Number 0) None
    | op == Div && (arg1 == arg2) = Oper Set d (Number 1) None
    | op == Div && (arg2 == Number 1) = Oper Set d arg1 None
    | op == Mod && arg1 == arg2 = Oper Set d (Number 0) None
    | op == Eql && arg1 == arg2 = Oper Set d (Number 1) None
    | otherwise = o




