module Model.Expression ( Expression (..)
                        , BinaryOp
                        , UnaryOp
                        , evaluate
                        , eliminateNames
                        , module Model.Field
                        ) where

import Control.Monad.Reader(Reader, ask, asks, runReader)
import Data.List(elemIndex)
import Model
import Model.Field

data Expression = Position Int
                | NamedPosition String
                | Constant Field
                | Unary UnaryOp Expression
                | Binary BinaryOp Expression Expression
                | Error String

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

eliminateNames :: Model -> Expression -> Expression
eliminateNames m exp = runReader (transform noNames exp) m
    where noNames :: Expression -> Reader Model Expression
          noNames (NamedPosition name) = do
              m <- ask
              return $ case elemIndex name (fnames m) of
                 Nothing -> Error $ "Mal nombre de campo: " ++ name
                 Just i -> Position i
          noNames n = return n

