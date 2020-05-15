{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Expression.Manipulate ( eliminateNames
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

-- |Changes all the `NamedPosition` in a `Expression` to the corresponding `Position`.
eliminateNames :: RowStore -> Expression -> Expression
eliminateNames rst (In (NamedPosition name)) = In $ case fieldIndex rst name of
                                                 Nothing -> Error $ "Mal nombre de campo: " `T.append` name
                                                 Just i -> Position i
eliminateNames rst (In (FromSource s inr ins gets)) = In $ case identifySource rst s of
    Nothing -> Error "Mal nombre de fuente"
    Just (rst', s') -> let
                         inr' = eliminateNames rst inr
                         ins' = eliminateNames rst' ins
                         gets' = eliminateNames rst' gets
                       in FromSource s' inr' ins' gets'
eliminateNames rst n = In (fmap (eliminateNames rst) (out n))

identifySource :: RowStore -> Expression -> Maybe (RowStore, Expression)
identifySource rst e@(In (Position n)) = Just (getRowStore rst n, e)
identifySource rst (In (NamedPosition name)) = do
    n <- getRowStoreIndex rst name
    return (getRowStore rst n, In (Position n))
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
    where tNames (In (NamedPosition name)) = do
              let name' = fromMaybe name (lookup name newNames)
              when (name' /= name) $ tell (Any True)
              return . In $ NamedPosition name'
          tNames e = return e

