{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Benchimpo.ExprParse
  ( parseExpr
  ) where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Number (int, floating)
import Data.Char (isSpace)
import Data.Maybe
import qualified Data.Map as Map
import Benchimpo.Expr

eqFactor :: Parser Expr
eqFactor = try fnCall <|> timeVar <|> paren <|> try floatNum <|> intNum
  where fnCall = do
          sym <- many1 letter
          arg <- paren
          return (FnCall sym arg)
        timeVar = do
          char 't'
          return (Var "t")
        paren = do
          char '('
          expr <- eqExpr
          char ')'
          return expr
        floatNum = do
          f <- floating
          return (RealNum f)
        intNum = do
          n <- int
          return (RealNum $ fromIntegral (n :: Int))

eqPower :: Parser Expr
eqPower = do
  factor <- eqFactor
  power factor <|> return factor
  where power factor = do
            char '^'
            p <- eqPower
            return (Pow factor p)

termOps :: Map.Map Char (Expr -> Expr -> Expr)
termOps = Map.fromList
  [ ('*', Mul)
  , ('/', Div)
  ]

eqTerm :: Parser Expr
eqTerm = do
  power <- eqPower
  let term = do
        op <- oneOf (Map.keys termOps)
        t <- eqTerm
        return $ (fromJust $ Map.lookup op termOps) power t
  term <|> return power

exprOps :: Map.Map Char (Expr -> Expr -> Expr)
exprOps = Map.fromList
  [ ('+', Add)
  , ('-', Sub)
  ]

eqExpr :: Parser Expr
eqExpr = do
  term <- eqTerm
  let expr = do
        op <- oneOf (Map.keys exprOps)
        e <- eqExpr
        return $ (fromJust $ Map.lookup op exprOps) term e
  expr <|> return term

exprLine :: Parser Expr
exprLine = do
  expr <- eqExpr
  eof
  return expr

parseExpr :: String -> Either ParseError Expr
parseExpr expr = parse exprLine expr trimmed
  where trimmed = filter (not . isSpace) expr
