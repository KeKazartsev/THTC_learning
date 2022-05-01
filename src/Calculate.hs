module Calculate (
    execIR,
    calcOper,
) where

import Reg
import Oper
import IR
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)

calcOper :: OpName -> Int -> Int -> Int
calcOper op n1 n2
    | op == Add = n1 + n2
    | op == Mul = n1 * n2
    | op == Div = if n1 * n2 >= 0 then div n1 n2 else -(div (abs n1) (abs n2))
    | op == Mod = if n1 >= 0 && n2 > 0 then mod n1 n2 else undefined
    | op == Eql = if n1 == n2 then 1 else 0
    | op == Set || op == GlobalUse = n1

execOper o@(Oper op d u1 u2) calcR = def
    where
    v1 = getVal calcR u1
    v2 = getVal calcR u2
    def = calcOper op (fromJust v1) (fromJust v2)

execIOpers stack os = go os calcR calcI
    where
    calcR = M.fromList $ zip (map stackReg [0..]) stack
    calcI = M.empty
    go [] calcR calcI= (calcR, calcI)
    go ((i, o@(Oper op d u1 u2)):os) calcR calcI = go os calcR' calcI'
        where
        rs = execOper o calcR
        calcR' = setReg calcR d rs
        calcI' = M.insert i rs calcI

execIR :: [Int] -> IR -> M.Map Int Int
execIR stack ir = snd $ execIOpers stack (_iOpers ir)
