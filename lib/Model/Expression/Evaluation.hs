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
eval = hookedCataM hook ev
    where
      ev (Position n) = evalIndex n
      ev (NamedPosition name) = return . mkError $ "Expresión con variable: " `T.append` name
      ev (Constant f) = return f
      ev (Unary info v) = return $ opU info v
      ev (Binary info v1 v2) = return $ opB info v1 v2
      ev (PrefixBinary info v1 v2) = return $ opPB info v1 v2
      ev (Cast ft v) = return $ convert ft v
      ev (Ternary v1 v2 v3) = return $ ternary v1 v2 v3
      ev (FromSource si n1 n2 n3) = undefined
      ev (Error m) = return $ mkError m

      hook (FromSource si n1 n2 n3) _ = evalFromSource si n1 n2 n3
      hook _ v = v

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

