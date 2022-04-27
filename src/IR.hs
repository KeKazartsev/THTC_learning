{-# LANGUAGE TemplateHaskell #-}
module IR (
    IRCnt(..), opN, vrN, srN,
    IR(..), iOpers, iRcnt, iRstat, gDefs, ests, nPhase, nPhaseNum,
    saveIR,
    collectIRStats,
    IRInit(..),
    IRPhase(..),
    iRNormalizePhase,
) where

import Reg
import Oper

import Lib

import Text.Printf
import Text.Parsec (ParseError)
import Control.Lens
import Data.Either (isRight)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (sort)
import Data.Maybe (catMaybes)
import Estimate

data IRPhase = IRPhCreate | IRPh2VirtRegs | IRPhDCE deriving (Show, Enum, Eq)

data IRRel = IRRel {
    _gDefs1 :: M.Map Var Var,
    _oCalcs1 :: M.Map Var Int,
    _oEsts1 :: M.Map Var (Int, Int)
}
makeLenses ''IRRel

data IRCnt = IRCnt {
    _opN :: Int,
    _vrN :: Int,
    _srN :: Int,
    _srs :: [Int]
}
makeLenses ''IRCnt
showIRCnts :: IRCnt -> String
showIRCnts cnts = printf "Statistic :\n\tOpers Used = %s\n\tVirtual Regs Used = %s\n\tStack Regs Used = %s\n\tStack Regs = %s" (show $ _opN cnts) (show $ _vrN cnts) (show $ _srN cnts) (show $ _srs cnts)

data IR = IR {
    _nPhase :: IRPhase,
    _nPhaseNum :: Int,
    _iOpers :: [IOper],
    _gDefs :: M.Map Var Var,
    _vars :: M.Map Var Int,
    _ests :: M.Map Var EstimateRes,
    _iRcnt :: IRCnt,
    _iRstat :: IRCnt
}
makeLenses ''IR

showIR :: IR -> String
showIR ir = printf "%s\n%s\n%s\n%s\n\n\n%s\n%s\n\n%s" sNPhase sPhase sIOCount sStat sIOpers'GD sGDefs sEsts
    where
    sNPhase = printf "PhaseNumber = %d" (_nPhaseNum ir) :: String
    sPhase = "IR Phase = " ++ show (_nPhase ir) :: String
    sIOCount = printf "Opers Count = %d" (length $ _iOpers ir) :: String

    sGDefs = show $ _gDefs ir
    sEsts = show $ _ests ir
    sStat = showIRCnts (_iRstat ir) :: String

    sIOpers'GD = unlines $ if _nPhase ir == IRPhCreate
        then showIROpers1 ir else showIROpers2 ir
    sIOpersGD = printf "\n\nOpers(Global Defs):\n%s\n\n" sIOpers'GD :: String

showIROpers1 ir = map (cutQuotes . show . f) (_iOpers ir)
    where f iop = showIOperAdvanced [showIOperGDef] "|" iop ir
showIROpers2 ir = map (cutQuotes . show . f) (_iOpers ir)
    where f iop = showIOperAdvanced [showIOperGDef, showIOperEst] "|" iop ir

type FShowIOperAttribute = String -> IOper -> IR -> String -> String

showIOperAttributes :: [FShowIOperAttribute] -> String -> IOper -> IR -> String -> String
showIOperAttributes [] _ _ _ str = str
showIOperAttributes (f:fs) dlm iop ir str = showIOperAttributes fs dlm iop ir (f dlm iop ir str)

showIOperAdvanced :: [FShowIOperAttribute] -> String -> IOper -> IR -> String
showIOperAdvanced fs dlm iop ir = showIOperAttributes fs dlm iop ir (showIOper iop)

showIOperGDef dlm (_, Oper op d u1 u2) ir str = printf "%s %s %1s" str dlm attr
    where attr = case M.lookup d (_gDefs ir) of
            Nothing -> ""
            Just g -> show g

showIOperEst dlm (_, Oper op d u1 u2) ir str = printf "%s %s %s" str dlm attr
    where attr = case M.lookup d (_ests ir) of
            Nothing -> " - "
            Just Nothing -> " NAN "
            Just (Just val) -> show val




saveIR file_prefix ir = do
    let fileName = printf "%s_ir_%03d_%s.txt" file_prefix (_nPhaseNum ir) (show $ _nPhase ir)
    writeFile fileName (showIR ir)

collectIRStats :: [Oper] -> IRCnt
collectIRStats opers = IRCnt(length opers) (S.size vrs) (S.size srs) (S.toList srs)
    where
    (vrs, srs) = go opers S.empty S.empty
    go [] vs ss = (vs, ss)
    go (Oper op d u1 u2:os) vs ss = go os vs' ss'
        where
        vs' = if isRegVirt d then S.insert d vs else vs
        ss' = if op == Set && isValRegStack u1
            then S.insert (getValRegIndexStrict u1) ss else ss

type IRInit = ([Oper], Int)

-- === Utility IR work
findArg ops (Number n) = Nothing
findArg ops None = Nothing
findArg ops (RVar v) = findDef ops v

findDef [] _ = Nothing
findDef ((i, o@(Oper op d _ _)):os) u = if u == d then Just o else findDef os u


-- === Normalize Phase
normalizeIOpers ios = map (\(i, o) -> (i, normalizeOper o)) ios
normalizeOper o@(Oper op d (Number n) (RVar (Var v))) =
    if op == Add || op == Mul || op == Eql
    then Oper op d (RVar (Var v)) (Number n) else o
normalizeOper o = o

iRNormalizePhase ir = (iRstat .~ stats') $ ir1
    where
    ir1 = (iOpers %~ normalizeIOpers) $ estimateIR ir
    stats' = collectIRStats (snd $ unzip $ _iOpers ir)

estimateIR :: IR -> IR
estimateIR ir = (ests .~ est') ir
    where est' = estimateOpers $ _iOpers ir
