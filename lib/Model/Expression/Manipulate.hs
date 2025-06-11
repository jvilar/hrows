{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Expression.Manipulate ( addPositions
                                     , translatePositions
                                     , translateNames
                                     , getPositions
                                     , asPosition
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
addPositions rst (In (SourceName name _)) = case getRowStoreIndex rst name of
    Nothing -> mkErrorExpr $ "Mal nombre de fuente: " `T.append` name
    Just rst -> mkKnownSourceName name rst
addPositions rst (In (FromSource s inr ins gets)) = case s' of
    In (SourceName _ (Just n)) -> let
                                     rst' = getRowStore rst n
                                     inr' = addPositions rst inr
                                     ins' = addPositions rst' ins
                                     gets' = addPositions rst' gets
                                  in mkFromSource s' inr' ins' gets'
    _ -> s'
   where s' = addPositions rst s
addPositions rst (In n) = In (fmap (addPositions rst) n)

type Changed = Bool

-- |Changes the absolute references according to the list of new positions.
-- Returns True if any position changed.
translatePositions :: [Int] -> Expression -> (Expression, Changed)
translatePositions newPos = second getAny . runWriter . bottomUpM tPos
    where tPos :: Node Expression -> Writer Any (Node Expression)
          tPos (Position n) = do
              let n' = newPos !! n
              when (n' /= n) $ tell (Any True)
              return $ Position n'
          tPos e = return e

translateNames :: [(Text, Text)] -> Expression -> (Expression, Changed)
translateNames newNames = second getAny . runWriter . bottomUpM tNames
    where tNames (NamedPosition name _) = do
              let name' = fromMaybe name (lookup name newNames)
              when (name' /= name) $ tell (Any True)
              return $ NamedPosition name' Nothing
          tNames e = return e

-- |If the expression references a single position (i.e. it is a `Position` or a `NamedPosition`),
-- it returns the position. Otherwise it returns Nothing.
asPosition :: Expression -> Maybe Int
asPosition (In (Position n)) = Just n
asPosition (In (NamedPosition _ (Just n))) = Just n
asPosition _ = Nothing

