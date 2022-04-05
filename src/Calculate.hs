module Calculate (
    execIR
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

execOper regs o@(Oper op d u1 u2) = regs'
    where
    v1 = getVal regs u1
    v2 = getVal regs u2
    def = calcOper op (fromJust v1) (fromJust v2)
    regs' = setReg regs d def

execOpers stack os = go regs os []
    where
    regs = M.fromList $ zip (map stackReg [0..]) stack
    go regs [] res = res
    go regs ((i, o@(Oper op d u1 u2)):os) res = go regs' os res'
        where
        regs' = execOper regs o
        res' = if op == GlobalUse
            then res ++ [(d, getRegStrict regs' d)] else res

execIR :: [Int] -> IR -> [(Var, Int)]
execIR stack ir = execOpers stack (_iOpers ir)
