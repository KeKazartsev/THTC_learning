module Oper (
    OpName(..), Oper(..),
    IOper, showIOper, showIOperR,
    operDef, operArg1, operArg2,
) where

import Reg
import qualified Data.Map.Strict as M
import Text.Printf

type IOper = (Int, Oper)
showIOper :: IOper -> String
showIOper (i, o) = printf "%5d: %s" i (show o)

showIOperR :: Show a => M.Map Var a -> IOper -> String
showIOperR gdefs (i, o@(Oper _ d _ _)) = case M.lookup d gdefs of
    Nothing -> showIOper (i, o)
    Just g -> printf "%s(== %s)" (showIOper (i, o)) (show g)

data OpName = GlobalUse | Inp | Set | Add | Sub | Mul | Div | Mod | Eql deriving (Eq, Ord, Show)

data Oper = Oper OpName Var Val Val deriving (Eq, Ord)
instance Show Oper where
    show (Oper GlobalUse d u1 _) = printf "%s <- %s %s" (show d) (show GlobalUse) (show u1)
    show (Oper Set d u1 _) = printf "%s <- %s %s" (show d) (show Set) (show u1)
    show (Oper op d u1 u2) = printf "%s <- %s %s %s" (show d) (show u1) (show op) (show u2)

operDef (Oper _ d _ _) = d
operArg1 (Oper _ _ u1 _) = u1
operArg2 (Oper _ _ _ u2) = u2
