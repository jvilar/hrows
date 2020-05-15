{-# LANGUAGE OverloadedStrings #-}

module Model (
              -- *Types
              Model
              , ModelChanged
              -- *Functions
              , setStore
              , empty
              , from
              , (<@)
              , inside
              , fromRowStore
              , addSource
              -- *Rexported
              , module Model.RowStore
) where

import Model.RowStore hiding (empty)
import qualified Model.RowStore.Update as RS

-- |A `Model` contains a `RowStore` with the data of the application
-- and a list of `Source`.
data Model = Model { _rowStore :: RowStore
                   , _sources :: [RowStore]
                   } deriving Show
                   
-- |Change the `RowStore` of the model
setStore :: RowStore -> Model -> Model
setStore rst m = m { _rowStore = rst } 
                   
-- |An empty `Model`
empty :: Model
empty = fromRowStore (RS.empty "vacía")

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
addSource :: RowStore -> Model -> Model
addSource rst m = m {
                      _rowStore = addRowStore rst $ _rowStore m 
                      , _sources = rst : _sources m
                    }