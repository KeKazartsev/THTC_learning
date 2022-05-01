module Parse (
    doParseIR, doParse, Text.Parsec.ParseError, Data.Either.fromRight,
) where

import IR
import Oper
import Reg

import Text.Parsec.Char as PC
import Text.Parsec hiding (State)
import Control.Monad.State
import Control.Monad.Identity

import Data.Either (isRight, fromRight)
import qualified Data.Map.Strict as M
import Data.List (nub, maximumBy)
import Data.Maybe (catMaybes)
import Control.Lens

type StateParse i o = ParsecT i () (State (Int)) o

doParse :: String -> Either ParseError IRInit
doParse str = case og of
    Left err -> Left err
    Right og' -> Right (og', si)
    where
    (og, si) = flip runState (0) $ runPT parseData () "" str1
    str1 = "set w 0\nset x 0\nset y 0\nset z 0\n" ++ str
    str2 = str

parseData :: StateParse String [Oper]
parseData = sepEndBy parseString (many1 newline)

parseString :: StateParse String Oper
parseString = choice [try parseGlobalUse, try parseInp, try parseAdd, try parseMul, try parseDiv, try parseMod, try parseEql, try parseSet]

parseInp :: StateParse String Oper
parseInp = do
    _ <- string "inp "
    d <- oneOf "wxyz"
    si <- get
    let sr = stackReg si
    put $ si + 1
    return $ Oper Set (Var [d]) (RVar sr) None

parseSet :: StateParse String Oper
parseSet = do
    _ <- string "set "
    d <- oneOf "wxyz"
    _ <- char ' '
    u1 <- parseVal
    return $ Oper Set (Var [d]) u1 None

parseGlobalUse :: StateParse String Oper
parseGlobalUse = do
    _ <- string "globaluse "
    d <- oneOf "wxyz"
    return $ Oper GlobalUse (Var [d]) (RVar $ Var [d]) None

parseAdd = parseOper "add " Add
parseMul = parseOper "mul " Mul
parseDiv = parseOper "div " Div
parseMod = parseOper "mod " Mod
parseEql = parseOper "eql " Eql
parseOper :: String -> OpName -> StateParse String Oper
parseOper str oper = do
    _ <- string str
    var1 <- parseVar
    _ <- char ' '
    val2 <- parseVal
    return $ Oper oper var1 (RVar var1) val2

parseVal :: StateParse String Val
parseVal = parsecMap regToVal parseVar <|> parseNumber

parseVar = parsecMap (\r -> Var [r]) $ oneOf "wxyz"
parseNumber = parseNegNumber <|> parsePosNumber
parsePosNumber = parsecMap (\n -> Number (read n)) $ many1 digit
parseNegNumber = do
    _ <- char '-'
    Number n <- parsePosNumber
    return $ Number (-n)

-- === PARSE IR
doParseIR :: [String] -> String -> Either ParseError IR
doParseIR guses str = do
    ir1 <- doParse (str ++ "\n" ++ unlines guses)
    let ir2 = parseToIR ir1
    return ir2

collectGUses :: [IOper] -> M.Map Int Var
collectGUses ios = go ios M.empty
    where
    go [] gu = gu
    go ((i, o@(Oper op d _ _)):os) gu = go os gu'
        where gu' = if op == GlobalUse then M.insert i d gu else gu

parseToIR :: IRInit -> IR
parseToIR (o_gv, srn) = (iOpers .~ io) . (gUses .~ gu) . (iRcnt .~ ir_cnts) . (iRstat .~ ir_stats) $ emptyIR
    where
    io = zip [0..] o_gv
    gu = collectGUses io
    ir_cnts = IRCnt (length io) 0 srn [0..pred srn]
    ir_stats = IRCnt (length io) 0 srn [0..pred srn]
