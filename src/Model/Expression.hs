module Model.Expression ( Expression (..)
                        , Formula
                        , BinaryOp
                        , UnaryOp
                        , BinaryOpInfo(..)
                        , UnaryOpInfo(..)
                        , Priority
                        , Associativity(..)
                        , addCast
                        , evaluate
                        , eliminateNames
                        , getPositions
                        , toFormula
                        , module Model.Field
                        ) where

import Control.Monad.Reader(Reader, ask, runReader)
import Data.Char(isAlphaNum)
import Data.List(elemIndex)
import Model.Field
import Model.Row

-- |The Formula is the expression as written by the user.
type Formula = String

-- |The priority of an operator
type Priority = Int

-- |The associativity of a binary operator
data Associativity = LeftAssoc | RightAssoc | TrueAssoc | NoAssoc

-- |The Expression is the internal representation of the Formula.
data Expression = Position Int
                | NamedPosition String
                | Constant Field
                | Unary UnaryOpInfo Expression
                | Binary BinaryOpInfo Expression Expression
                | Cast FieldType Expression
                | Error String
                deriving Show

type UnaryOp = Field -> Field

-- |The information stored about a unary operation.
data UnaryOpInfo = UnaryOpInfo { opU :: UnaryOp
                               , formulaU :: Formula
                               }

instance Show UnaryOpInfo where
    show = formulaU

type BinaryOp = Field -> Field -> Field

-- |The information stored about a binary operation.
data BinaryOpInfo = BinaryOpInfo { opB :: BinaryOp
                                 , formulaB :: Formula
                                 , prioB :: Priority
                                 , assocB :: Associativity
                                 }

instance Show BinaryOpInfo where
    show = formulaB

transform :: Monad m => (Expression -> m Expression) -> Expression -> m Expression
transform t (Unary info e) = do
    e' <- transform t e
    t (Unary info e')
transform t (Binary info e1 e2) = do
    e1' <- transform t e1
    e2' <- transform t e2
    t (Binary info e1' e2')
transform t (Cast ft e) = do
    e' <- transform t e
    t (Cast ft e')
transform t n = t n

evaluate :: Row -> Expression -> Field
evaluate r exp = runReader (eval exp) r

toFormula :: Expression -> Formula
toFormula (Position p) = "$" ++ show (p + 1)
toFormula (NamedPosition n) | all isAlphaNum n = n
                            | otherwise = "@{" ++ n ++ "}"
toFormula (Constant f) | typeOf f == TypeString = '"' : toString f ++ "\""
                       | otherwise = toString f
toFormula (Unary info e) = formulaU info ++ parent 8 e
toFormula (Binary info e1 e2) = let
    (pe1, pe2) = case assocB info of
                     LeftAssoc -> (prioB info, prioB info + 1)
                     RightAssoc -> (prioB info + 1, prioB info)
                     TrueAssoc -> (prioB info, prioB info)
                     NoAssoc -> (prioB info + 1, prioB info + 1)
    in parent pe1 e1 ++ formulaB info ++ parent pe2 e2
toFormula (Cast ft e) = typeOperator ft ++ "(" ++ toFormula e ++ ")"
toFormula (Error s) = "Error: " ++ s

parent :: Priority -> Expression -> String
parent p e@(Binary info _ _ ) | prioB info >= p = toFormula e
                              | otherwise = "(" ++ toFormula e ++ ")"
parent _ e = toFormula e

type Eval = Reader Row
eval :: Expression -> Eval Field
eval (Position n) = evalIndex n
eval (NamedPosition name) = return . mkError $ "Expresión con variable: " ++ name
eval (Constant f) = return f
eval (Unary info exp) = opU info <$> eval exp
eval (Binary info exp1 exp2) = opB info <$> eval exp1 <*> eval exp2
eval (Cast ft exp) = convert ft <$> eval exp
eval (Error m) = return $ mkError m

evalIndex :: Int -> Eval Field
evalIndex n = do
    r <- ask
    return $ if 0 <= n && n < length r
             then r !! n
             else mkError $ "Índice erróneo " ++ show (n + 1)

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

