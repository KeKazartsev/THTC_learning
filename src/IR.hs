{-# LANGUAGE TemplateHaskell #-}
module IR (
    IRCnt(..), opN, vrN, srN,
    IR(..), iOpers, iRcnt, iRstat, gDefs, nPhase, nPhaseNum,
    saveIR,
    collectIRStats,
    IRInit(..),
    IRPhase(..),
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

-- === Opers
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
    _ests :: M.Map Var (Int, Int),
    _iRcnt :: IRCnt,
    _iRstat :: IRCnt
}
makeLenses ''IR

showIR :: IR -> String
showIR ir = printf "%s\n%s\n%s\n%s\n\n\n%s\n%s" sNPhase sPhase sIOCount sUsedCnts sIOpers'GD sGDefs
    where
    sNPhase = printf "PhaseNumber = %d" (_nPhaseNum ir) :: String
    sPhase = "IR Phase = " ++ show (_nPhase ir) :: String
    sIOCount = printf "Opers Count = %d" (length $ _iOpers ir) :: String

    sGDefs = show $ sort $ M.toList (_gDefs ir) :: String
    sUsedCnts = showIRCnts (_iRstat ir) :: String

    sIOpers'GD = unlines $ showIRIOpers _gDefs ir :: String
    sIOpersGD = printf "\n\nOpers(Global Defs):\n%s\n\n" sIOpers'GD :: String

showIRIOpers get_relation ir = map (cutQuotes . show . (showIOperR (get_relation ir))) (_iOpers ir)

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
