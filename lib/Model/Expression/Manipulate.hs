{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Expression.Manipulate ( addPositions
                                     , translatePositions
                                     , translateNames
                                     , getPositions
) where

import Control.Arrow(second)
import Control.Monad(when)
import Control.Monad.Writer(Writer, tell, runWriter)
import Data.Maybe(fromMaybe)
import Data.Monoid(Any(..))
import Data.Text(Text)
import qualified Data.Text as T

import Model.Expression
import Model.Expression.RecursionSchemas
import Model.RowStore.Base

-- |Adds the corresponding postitions to all the `NamedPosition` in a `Expression`.
addPositions :: RowStore -> Expression -> Expression
addPositions rst (In (NamedPosition name _)) = case fieldIndex rst name of
                                                 Nothing -> mkErrorExpr $ "Mal nombre de campo: " `T.append` name
                                                 Just n -> mkKnownNamedPosition name n
addPositions rst (In (FromSource s inr ins gets)) = case identifySource rst s of
    Nothing -> mkErrorExpr $ T.append "Mal nombre de fuente: " (toFormula s)
    Just (rst', s') -> let
                         inr' = addPositions rst inr
                         ins' = addPositions rst' ins
                         gets' = addPositions rst' gets
                       in mkFromSource s' inr' ins' gets'
addPositions rst n = In (fmap (addPositions rst) (out n))

identifySource :: RowStore -> Expression -> Maybe (RowStore, Expression)
identifySource rst (In (NamedPosition name _)) = do
    n <- getRowStoreIndex rst name
    return (getRowStore rst n, mkConstant $ toField n)
identifySource _ _ = Nothing


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
    where tNames (In (NamedPosition name _)) = do
              let name' = fromMaybe name (lookup name newNames)
              when (name' /= name) $ tell (Any True)
              return $ mkNamedPosition name'
          tNames e = return e

