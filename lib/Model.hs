{-# LANGUAGE OverloadedStrings #-}

module Model (
              -- *Types
              Model
              , ModelChanged
              -- *Functions
              , empty
              , setStore
              , from
              , (<@)
              , inside
              , fromRowStore
              , addSource
              , addSourceInfo
              , getSourceInfos
              , renameSources
              -- *Rexported
              , module Model.RowStore
              , module Model.SourceInfo
) where

import Model.Empty
import Model.RowStore
import Model.SourceInfo

-- |A `Model` contains a `RowStore` with the data of the application
-- and a list of `Source`.
data Model = Model { _rowStore :: RowStore
                   , _sources :: [SourceInfo]
                   } deriving Show

-- |Change the `RowStore` of the model
setStore :: RowStore -> Model -> Model
setStore rst m = m { _rowStore = rst }

instance Empty Model where
    empty = fromRowStore $ emptyName "vacía"

-- |An alias of `Bool` to indicate if the model has changed
type ModelChanged = Bool

-- |Create a `Model` from a `RowStore`
fromRowStore :: RowStore -> Model
fromRowStore rs = Model rs []

-- |Utility function to apply a function to the `RowStore` of the `Model`.
from :: (RowStore -> a) -> Model -> a
from = (. _rowStore)

-- |The operator form of `from`
(<@)  :: (RowStore -> a) -> Model -> a
(<@) = from

-- |Another utility function, this changes the `RowStore` of the `Model`.
inside :: (RowStore -> RowStore) -> Model -> Model
inside f m = setStore (f $ _rowStore m) m

-- |Adds a new source to the `Model`
addSource :: SourceInfo -> RowStore -> Model -> Model
addSource si rst m = m {
                         _rowStore = addRowStore rst $ _rowStore m
                         , _sources = si : _sources m
                       }

-- |Adds a 'SourceInfo' to the 'Model'
addSourceInfo :: SourceInfo -> Model -> Model
addSourceInfo si m = m { _sources = si : _sources m }

-- |Get the 'SourceInfo' for the sources
getSourceInfos :: Model -> [SourceInfo]
getSourceInfos = _sources

-- |Change the names of the sources
renameSources :: [SourceName] -> Model -> Model
renameSources names m = m {
                            _rowStore = renameDataSources names $ _rowStore m
                            , _sources = zipWith renameSourceInfo names $ _sources m 
                          }
