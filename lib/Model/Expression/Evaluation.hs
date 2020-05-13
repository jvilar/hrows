{-# LANGUAGE DeriveFoldable
           , DeriveFunctor
           , DeriveTraversable
           , FlexibleContexts
           , FlexibleInstances
           , OverloadedStrings
           , TypeSynonymInstances
#-}

module Model.Expression.Evaluation ( evaluate
                                   , eliminateNames
                                   , translatePositions
                                   , translateNames
                                   , getPositions
                                   ) where

import Control.Arrow(second, (>>>), (<<<))
import Control.Monad(when)
import Control.Monad.Reader(Reader, ask, runReader)
import Control.Monad.Writer(Writer, tell, runWriter)
import Data.Char(isAlphaNum)
import Data.Function((&))
import Data.List(elemIndex, find)
import Data.Maybe(fromMaybe)
import Data.Monoid(Any(..))
import Data.Text(Text)
import qualified Data.Text as T
import TextShow(TextShow(showt))

import Model.Expression
import Model.Expression.RecursionSchemas
import Model.Row

evaluate :: Row -> [DataSource] -> Expression -> Field
evaluate r rsts exp = runReader (eval exp) (r, rsts)

type Eval = Reader (Row, [DataSource])

eval :: Expression -> Eval Field
eval = cataM ev
    where
      ev (Position n) = evalIndex n
      ev (NamedPosition name) = return . mkError $ "Expresión con variable: " `T.append` name
      ev (Constant f) = return f
      ev (Unary info v) = return $ opU info v
      ev (Binary info v1 v2) = return $ opB info v1 v2
      ev (PrefixBinary info v1 v2) = return $ opPB info v1 v2
      ev (Cast ft v) = return $ convert ft v
      ev (Ternary v1 v2 v3) = return $ ternary v1 v2 v3
      ev (FromSource si n1 n2 n3) = evalFromSource si n1 n2 n3
      ev (Error m) = return $ mkError m

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

evalFromSource :: Field -> Field -> Field -> Field -> Eval Field
evalFromSource si n1 n2 n3 = do
    (r, rsts) <- ask
    let t = do
                rst <- recover rsts (toInt si) "Fuente errónea"
                v <- recover r (toInt n1) "Índice de búsqueda erróneo"
                case find (posEqual (toInt n2) v) rst of 
                  Nothing -> Left "No encontrado"
                  Just r -> recover r (toInt n3) "Índice en recuperación erróneo"
    return $ case t of
        Right v -> v
        Left e -> mkError e

eliminateNames :: [Text] -> Expression -> Expression
eliminateNames fnames = bottomUp noNames
    where noNames (In (NamedPosition name)) = In $ case elemIndex name fnames of
                                                 Nothing -> Error $ "Mal nombre de campo: " `T.append` name
                                                 Just i -> Position i
          noNames n = n

type Changed = Bool

-- |Changes the absolute references according to the list of new positions.
-- Returns True if any position changed.
translatePositions :: [Int] -> Expression -> (Expression, Changed)
translatePositions newPos = second getAny . runWriter . bottomUpM tPos
    where tPos :: Expression -> Writer Any Expression
          tPos (In (Position n)) = do
              let n' = newPos !! n
              when (n' /= n) $ tell (Any True)
              return . In $ Position n'
          tPos e = return e

translateNames :: [(Text, Text)] -> Expression -> (Expression, Changed)
translateNames newNames = second getAny . runWriter . bottomUpM tNames
    where tNames (In (NamedPosition name)) = do
              let name' = fromMaybe name (lookup name newNames)
              when (name' /= name) $ tell (Any True)
              return . In $ NamedPosition name'
          tNames e = return e

getPositions :: Expression -> [Int]
getPositions = cata gp
    where
        gp (Position n) = [n]
        gp (NamedPosition name) = error $ "Expresión con variable: " ++ T.unpack name
        gp (Constant _) = []
        gp (Unary _ ps) = ps
        gp (Binary _ ps1 ps2) = merge ps1 ps2
        gp (PrefixBinary _ ps1 ps2) = merge ps1 ps2
        gp (Cast _ ps) = ps
        gp (Ternary ps1 ps2 ps3) = ps1 `merge` ps2 `merge` ps3
        gp (Error _) = []

merge :: [Int] -> [Int] -> [Int]
merge [] l = l
merge l@(_:_) [] = l
merge (x:xs) (y:ys) = case compare x y of
                          LT -> x : merge xs (y:ys)
                          EQ -> x : merge xs ys
                          GT -> y : merge (x:xs) ys
