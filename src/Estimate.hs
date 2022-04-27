module Estimate (
    EstimateRes,
    estimateOpers,
) where

import Reg
import Oper
import qualified Data.Map.Strict as M
import Text.Printf

{- EstimateRes: Just (minValue, maxValue) | Nothing
 - Nothing is mean "some not permitted result" : infinity or NAN
 -
 - Despite there is no Inf || NAN execution results in starting task we have to support it because of two reasons:
 - 1. Estimation is less precise than execution, but we have to "remember" all possible results, in price of "FalsePosibive" (count some impossible results as possible)
 - 2. Our goals is slight more general than "solve initial task" - but learn Haskell throug writing small DSL-compiler
 -
 - So the proper way is support "infinity || NAN" result
 -}
type EstimateRes = Maybe (Int, Int)

estimateOpers :: [IOper] -> M.Map Var EstimateRes
estimateOpers ops = go ops M.empty
    where
    go [] est = est
    go ((_, o@(Oper op d u1 u2)):os) est = go os est'
        where est' = setReg est d $ estimateOper o est

estimateOper :: Oper -> M.Map Var EstimateRes -> EstimateRes
estimateOper o@(Oper op d u1 u2) est = res
    where
    u1' = getEstimate est u1
    u2' = getEstimate est u2
    res = estimateOperValue op u1' u2'

getEstimate :: M.Map Var EstimateRes -> Val -> EstimateRes
getEstimate _ None = error "Try to estimate Empty operand"
getEstimate _ (Number n) = Just (n, n)
getEstimate est (RVar (Var v))
    | isRegStack (Var v) = Just (1, 9)
    | otherwise = getRegStrict est (Var v)

estimateOperValue :: OpName -> EstimateRes -> EstimateRes -> EstimateRes
estimateOperValue Set arg1 _ = arg1
estimateOperValue GlobalUse arg1 _ = arg1
estimateOperValue op Nothing _ = if op == Eql then Just (0, 1) else Nothing
estimateOperValue op _ Nothing = if op == Eql then Just (0, 1) else Nothing
estimateOperValue op (Just (mn1, mx1)) (Just (mn2, mx2))
    | mn1 > mx1 || mn2 > mx2 = error $ printf "wrong estimation values for: %s %s %s" (show op) (show (Just (mn1, mx1))) (show (Just (mn2, mx2)))
    | op == Add = Just (mn1 + mn2, mx1 + mx2)
    | op == Sub = Just (mn1 - mx2, mx1 - mn2)
    | op == Eql = Just (minimum lst_eq, maximum lst_eq)
    | op == Mod = if mn1 >= 0 && mn2 > 0
        then Just (0, min mx1 (mx2 - 1)) else Nothing
    | op == Mul = Just (minimum lst_mul, maximum lst_mul)
    | op == Div = if isInPair (mn2, mx2) 0
        then Nothing else Just (minimum lst_div, maximum lst_div)
    where
    lst_div = [div i1 i2 | i1 <- l1, i2 <- l2]
    lst_mul = [i1 * i2 | i1 <- l1, i2 <- l2]
    lst_eq = [if i1 == i2 then 1 else 0 | i1 <- l1, i2 <- l2]
    l1 = [mn1, mx1] ++ filter(isInPair (mn1, mx1)) [-1, 0, 1]
    l2 = [mn2, mx2] ++ filter(isInPair (mn2, mx2)) [-1, 0, 1]

isInPair (mn, mx) val = mn <= val && mx >= val
