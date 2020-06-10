{-# LANGUAGE DeriveFoldable
           , DeriveFunctor
           , DeriveTraversable
           , FlexibleContexts
           , FlexibleInstances
           , OverloadedStrings
           , TypeSynonymInstances
#-}

module Model.Expression.Evaluation ( evaluate
                                   ) where

import Control.Monad(msum)
import Control.Monad.Reader(Reader, ask, runReader)
import Data.List(find)
import qualified Data.Text as T
import TextShow(TextShow(showt))

import Model.Expression
import Model.Expression.RecursionSchemas
import Model.Row

evaluate :: Row -> [DataSource] -> Expression -> Field
evaluate r dss exp = runReader (eval exp) (r, dss)

type Eval = Reader (Row, [DataSource])

eval :: Expression -> Eval Field
eval = para ev
    where
      ev :: RAlgebra Node (Eval Field)
      ev _ (Position n) = evalIndex n
      ev _ (NamedPosition name) = return . mkError $ "Expresión con variable: " `T.append` name
      ev _ (Constant f) = return f
      ev _ (Unary info v) = opU info <$> v
      ev _ (Binary info v1 v2) = opB info <$> v1 <*> v2
      ev _ (PrefixBinary info v1 v2) = opPB info <$> v1 <*> v2
      ev _ (Cast ft v) = convert ft <$> v
      ev _ (Ternary v1 v2 v3) = ternary <$> v1 <*> v2 <*> v3
      ev _ (ErrorCheck v1 v2) = do
                    q <- isError <$> v1
                    if q then v2 else v1
      ev (In (FromSource si n1 n2 n3)) _ =  evalFromSource si n1 n2 n3
      ev _ (Error m) = return $ mkError m

evalIndex :: Int -> Eval Field
evalIndex n = do
    (r, _) <- ask
    return $ case recover r n ("Índice erróneo " `T.append` showt (n + 1))  of
        Right v -> v
        Left e -> mkError e

recover :: [a] -> Int -> e -> Either e a
recover [] _ = Left
recover (x:_) 0 = const $ Right x
recover (_:xs) n = recover xs (n - 1)

posEqual :: Eq a => Int -> a -> [a] -> Bool
posEqual _ _ [] = False
posEqual 0 a (x:_) = a == x
posEqual n a (_:xs) = posEqual (n-1) a xs

evalFromSource :: Expression -> Expression -> Expression -> Expression -> Eval Field
evalFromSource si n1 n2 n3 = do
    source <- toInt <$> eval si
    v1 <- eval n1
    (_, dss) <- ask
    let t = do
              ds <- recover dss source $ "Fuente errónea: " `T.append` showt source
              case msum [
                     if v1 == evaluate row [] n2
                     then Just $ evaluate row [] n3
                     else Nothing 
                     | row <- ds
                    ] of
                 Just v -> Right v
                 Nothing -> Left $ "No encontrado " `T.append` (T.pack $ show v1)    
    return $ case t of
        Right v -> v
        Left e -> mkError e

