module Benchimpo.Expr where

import Data.Maybe
import qualified Data.Map as Map

class Equation a where
  evalExpr :: a -> Double -> Double

data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Pow Expr Expr
          | FnCall { fnName  :: String
                   , argExpr :: Expr}
          | RealNum Double
          | Var String

instance Show Expr where
  show (Add left right) = "(" ++ show left ++ " + " ++ show right ++ ")"
  show (Sub left right) = "(" ++ show left ++ " - " ++ show right ++ ")"
  show (Mul left right) = "(" ++ show left ++ " * " ++ show right ++ ")"
  show (Div left right) = "(" ++ show left ++ " / " ++ show right ++ ")"
  show (Pow left right) = "(" ++ show left ++ " ^ " ++ show right ++ ")"
  show (FnCall fn arg) = fn ++ "(" ++ show arg ++ ")"
  show (RealNum num) = show num
  show (Var sym) = sym

instance Equation Expr where
  evalExpr (Add left right) x = evalExpr left x + evalExpr right x
  evalExpr (Sub left right) x = evalExpr left x - evalExpr right x
  evalExpr (Mul left right) x = evalExpr left x * evalExpr right x
  evalExpr (Div left right) x = evalExpr left x / evalExpr right x
  evalExpr (Pow left right) x = evalExpr left x ** evalExpr right x
  evalExpr (FnCall fn arg) x = lookupFn fn $ evalExpr arg x
  evalExpr (RealNum num) _ = num
  evalExpr (Var _) x = x

functionsMap :: Map.Map String (Double -> Double)
functionsMap = Map.fromList
  [ ("sin", sin)
  , ("cos", cos)
  , ("tan", tan)
  , ("abs", abs)
  , ("floor", floor')
  , ("ceil", ceiling')
  , ("sqrt", sqrt)
  ]

floor' :: (RealFrac a) => a -> Double
floor' = fromIntegral . (floor :: RealFrac a => a -> Int)

ceiling' :: (RealFrac a) => a -> Double
ceiling' = fromIntegral . (ceiling :: RealFrac a => a -> Int)

lookupFn :: String -> Double -> Double
lookupFn name = fromMaybe id $ Map.lookup name functionsMap
