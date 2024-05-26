{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , OverloadedStrings
#-}

module Model.Expression.Evaluation ( evaluate
                                   ) where

import Control.Monad(msum)
import Control.Monad.Reader(Reader, ask, runReader)
import qualified Data.Text as T
import TextShow(TextShow(showt))

import Model.Expression
import Model.Expression.RecursionSchemas
import Model.Row

evaluate :: Row -> [DataSource] -> Expression -> Field
evaluate r dss ex = runReader (eval ex) (r, dss)

type Eval = Reader (Row, [DataSource])

eval :: Expression -> Eval Field
eval = para ev
    where
      ev :: RAlgebra Node (Eval Field)
      ev (Position n) = evalIndex n $ mkError $ "Error en $" `T.append` showt (n + 1)
      ev (NamedPosition name Nothing) = return . mkError $ "Expresión con variable desconocida: " `T.append` name
      ev (NamedPosition name (Just n)) = evalIndex n $ mkError $ "Error en " `T.append` name
      ev (Constant f) = return f
      ev (Unary info (_, v)) = opU info <$> v
      ev (Binary info (_, v1) (_, v2)) = opB info <$> v1 <*> v2
      ev (Prefix info vs) = opP info <$> sequence (map snd vs)
      ev (Cast ft vs) = convert ft <$> sequence (map snd vs)
      ev (Ternary (_, v1) (_, v2) (_, v3)) = ternary <$> v1 <*> v2 <*> v3
      ev (ErrorCheck (_, v1) (_, v2)) =
                    v1 >>= (\case False -> v1
                                  True  -> v2) . isError
      ev (FromSource (si, _) (v, _) (n1, _) (n2, _)) = evalFromSource si v n1 n2
      ev (Error m) = return $ mkError m

evalIndex :: Int -> Field -> Eval Field
evalIndex n inError = do
    (r, _) <- ask
    return $ case recover r n (mkError $ "Índice erróneo " `T.append` showt (n + 1))  of
        Right v -> if isError v
                   then inError
                   else v
        Left e -> e

recover :: [a] -> Int -> e -> Either e a
recover [] _ = Left
recover (x:_) 0 = const $ Right x
recover (_:xs) n = recover xs (n - 1)

evalFromSource :: Expression -> Expression -> Expression -> Expression -> Eval Field
evalFromSource si val n1 n2 = do
    source <- toInt <$> eval si
    v1 <- eval val
    (_, dss) <- ask
    let t = do
              ds <- recover dss source $ "Fuente errónea: " `T.append` toFormula si
              case msum [
                     if v1 == evaluate row [] n1
                     then Just $ evaluate row [] n2
                     else Nothing
                     | row <- ds
                    ] of
                 Just v -> Right v
                 Nothing -> Left $ "No encontrado " `T.append` toString v1
    return $ case t of
        Right v -> v
        Left e -> mkError e
