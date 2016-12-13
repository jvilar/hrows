module Model.Expression ( Expression (..)
                        , Formula
                        , BinaryOp
                        , UnaryOp
                        , Priority
                        , addCast
                        , evaluate
                        , eliminateNames
                        , getPositions
                        , module Model.Field
                        ) where

import Control.Monad.Reader(Reader, ask, runReader)
import Data.List(elemIndex)
import Model.Field
import Model.Row

-- |The Formula is the expression as written by the user.
type Formula = String

-- |The priority of an operator
type Priority = Int

-- |The Expression is the internal representation of the Formula.
data Expression = Position Int
                | NamedPosition String
                | Constant Field
                | Unary String UnaryOp Expression
                | Binary Priority String BinaryOp Expression Expression
                | Cast FieldType Expression
                | Error String

instance Show Expression where
    show (Position n) = "Position " ++ show n
    show (NamedPosition s) = "NamedPosition " ++ show s
    show (Constant f) = "Constant " ++ show f
    show (Unary str _ e) = "Unary " ++ str ++ " (" ++ show e ++ ")"
    show (Binary _ str _ e1 e2) = "Binary " ++ str ++ " (" ++ show e1 ++ ") (" ++ show e2 ++ ")"
    show (Cast ft e) = "Cast " ++ show ft ++ " (" ++ show e ++ ")"
    show (Error s) = "Error " ++ show s

type UnaryOp = Field -> Field

type BinaryOp = Field -> Field -> Field


transform :: Monad m => (Expression -> m Expression) -> Expression -> m Expression
transform t (Unary str op e) = do
    e' <- transform t e
    t (Unary str op e')
transform t (Binary pr str op e1 e2) = do
    e1' <- transform t e1
    e2' <- transform t e2
    t (Binary pr str op e1' e2')
transform t (Cast ft e) = do
    e' <- transform t e
    t (Cast ft e')
transform t n = t n

evaluate :: Row -> Expression -> Field
evaluate r exp = runReader (eval exp) r

type Eval = Reader Row
eval :: Expression -> Eval Field
eval (Position n) = evalIndex n
eval (NamedPosition name) = return . mkError $ "Expresión con variable: " ++ name
eval (Constant f) = return f
eval (Unary _ op exp) = op <$> eval exp
eval (Binary _ _ op exp1 exp2) = op <$> eval exp1 <*> eval exp2
eval (Cast ft exp) = convert ft <$> eval exp
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
getPositions (Unary _ _ exp) = getPositions exp
getPositions (Binary _ _ _ exp1 exp2) = merge (getPositions exp1) (getPositions exp2)
getPositions (Cast _ exp) = getPositions exp
getPositions (Error _) = []

merge :: [Int] -> [Int] -> [Int]
merge [] l = l
merge l@(_:_) [] = l
merge (x:xs) (y:ys) = case compare x y of
                          LT -> x : merge xs (y:ys)
                          EQ -> x : merge xs ys
                          GT -> y : merge (x:xs) ys

addCast :: FieldType -> Expression -> Expression
addCast ft exp@(Cast ft' e) | ft == ft' = exp
                            | otherwise = Cast ft e
addCast ft exp = Cast ft exp

