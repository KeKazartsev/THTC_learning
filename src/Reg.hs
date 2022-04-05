module Reg (
    Var(..), Val(..),
    isRegGlobal, isRegStack, isRegVirt,
    isValReg, isValRegGlobal, isValRegVirt, isValRegStack,
    getValRegStrict,
    getReg, getRegStrict, setReg,
    getVal,
    stackReg, virtReg,
    regToVal,
) where

import Lib
import qualified Data.Map.Strict as M

data Var = Var String deriving (Eq, Ord)
instance Show Var where
    show (Var v) =  cutQuotes (show v)

data Val = None | RVar Var | Number Int deriving (Eq, Ord)
instance Show Val where
    show None = "-"
    show (Number n) = show n
    show (RVar v) = show v

-- ======
stackReg i = Var $ 's':show i
virtReg i = Var $ 'v':show i

globalRegs = ["w", "x", "y", "z"]
isRegGlobal (Var v) = elem v ["w", "x", "y", "z"]

isRegVirt (Var ('v':_)) = True
isRegVirt _ = False

isRegStack (Var ('s':_)) = True
isRegStack _ = False

isValReg val = case val of
    RVar _ -> True
    otherwise -> False

isValRegGlobal val = case val of
    RVar v -> isRegGlobal v
    otherwise -> False

isValRegVirt val = case val of
    RVar v -> isRegVirt v
    otherwise -> False

isValRegStack val = case val of
    RVar v -> isRegStack v
    otherwise -> False

getValRegStrict val = case val of
    RVar v -> v
    otherwise -> error $ show val ++ " is not a reg"

-- GET && SET


getReg :: M.Map Var a -> Var -> Maybe a
getReg regs r = case M.lookup r regs of
    Just x -> Just x
    otherwise -> Nothing

getRegStrict :: Show a => M.Map Var a -> Var -> a
getRegStrict regs r = case getReg regs r of
    Just x -> x
    Nothing -> error $ "Can't get register \"" ++ show r ++ "\" from regs (" ++ show regs ++ ")"

setReg :: M.Map Var a -> Var -> a -> M.Map Var a
setReg regs r val = M.insert r val regs

getVal regs None = Nothing
getVal regs (Number n) = Just n
getVal regs (RVar v) = getReg regs v

--- regToVal
regToVal r = RVar r
