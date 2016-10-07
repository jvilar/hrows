module Model.Expression ( Expression (..)
                        , BinaryOp
                        , UnaryOp
                        , evaluate
                        , eliminateNames
                        , getPositions
                        , module Model.Field
                        ) where

import Control.Monad.Reader(Reader, ask, asks, runReader)
import Data.List(elemIndex)
import Model.Field
import Model.Row

data Expression = Position Int
                | NamedPosition String
                | Constant Field
                | Unary UnaryOp Expression
                | Binary BinaryOp Expression Expression
                | Error String

instance Show Expression where
    show (Position n) = "Position " ++ show n
    show (NamedPosition s) = "NamedPosition " ++ show s
    show (Constant f) = "Constant " ++ show f
    show (Unary _ e) = "Unary op (" ++ show e ++ ")"
    show (Binary _ e1 e2) = "Binary op (" ++ show e1 ++ ") (" ++ show e2 ++ ")"
    show (Error s) = "Error " ++ show s

type UnaryOp = Field -> Field

type BinaryOp = Field -> Field -> Field


transform :: Monad m => (Expression -> m Expression) -> Expression -> m Expression
transform t (Unary op e) = do
    e' <- t e
    t (Unary op e')
transform t (Binary op e1 e2) = do
    e1' <- t e1
    e2' <- t e2
    t (Binary op e1' e2')
transform t n = t n

evaluate :: Row -> Expression -> Field
evaluate r exp = runReader (eval exp) r

type Eval = Reader Row
eval :: Expression -> Eval Field
eval (Position n) = evalIndex n
eval (NamedPosition name) = return . mkError $ "Expresión con variable: " ++ name
eval (Constant f) = return f
eval (Unary op exp) = op <$> eval exp
eval (Binary op exp1 exp2) = op <$> eval exp1 <*> eval exp2
eval (Error m) = return $ mkError m

evalIndex :: Int -> Eval Field
evalIndex n = do
    r <- ask
    return $ if 0 <= n && n < length r
             then r !! n
             else mkError $ "Índice erróneo " ++ show n

eliminateNames :: [String] -> Expression -> Expression
eliminateNames fnames exp = runReader (transform noNames exp) fnames
    where noNames :: Expression -> Reader [String] Expression
          noNames (NamedPosition name) = do
              fnames <- ask
              return $ case elemIndex name fnames of
                 Nothing -> Error $ "Mal nombre de campo: " ++ name
                 Just i -> Position i
          noNames n = return n

getPositions :: Expression -> [Int]
getPositions (Position n) = [n]
getPositions (NamedPosition name) = error $ "Expresión con variable: " ++ name
getPositions (Constant f) = []
getPositions (Unary _ exp) = getPositions exp
getPositions (Binary _ exp1 exp2) = merge (getPositions exp1) (getPositions exp2)
getPositions (Error _) = []

merge :: [Int] -> [Int] -> [Int]
merge [] l = l
merge l@(_:_) [] = l
merge (x:xs) (y:ys) = case compare x y of
                          LT -> x : merge xs (y:ys)
                          EQ -> x : merge xs ys
                          GT -> y : merge (x:xs) ys
