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

eliminateNames :: RowStore -> Expression -> Expression
eliminateNames rst = bottomUp noNames
    where noNames (In (NamedPosition name)) = In $ case fieldIndex rst name of
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

