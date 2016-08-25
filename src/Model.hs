{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Model (
              -- *Types
              Model
             , Row
             -- *Classes
             , ToField(..)
             -- *Functions
             -- **Construction
             , empty
             , addRow
             , setNames
             , fromRows
             -- **Querying
             , names
             , row
             , size
             , toString
) where

import Data.IntMap.Strict(IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List(foldl')

-- |A field can store an Int, a Double or a String or it may be empty.
data Field = AInt Int
           | ADouble Double
           | AString String
           | Empty

class ToField t where
    toField :: t -> Field

instance ToField Int where
    toField = AInt

instance ToField Double where
    toField = ADouble

instance ToField String where
    toField = AString

instance ToField f => ToField (Maybe f) where
    toField Nothing = Empty
    toField (Just v) = toField v

-- |The string associated to a `Field`.
toString :: Field -> String
toString (AInt n) = show n
toString (ADouble d) = show d
toString (AString s) = s
toString Empty = "---"

-- |A row is a list of fields.
type Row = [Field]

-- |Holds the rows.
data Model = Model { _rows :: IntMap Row
                   , _names :: Maybe [String]
                   , _size :: Int
                   }

-- |An empty `Model`.
empty :: Model
empty = Model { _rows = IM.empty
              , _names = Nothing
              , _size = 0
              }

-- |Adds a `Row` to a `Model`.
addRow :: Model -> Row -> Model
addRow m r = m { _rows = IM.insert (_size m) r (_rows m)
               , _size = _size m + 1
               }

-- |Creates a model from a list of `Row`s.
fromRows :: [Row] -> Model
fromRows = foldl' addRow empty

-- |Sets the names of the field.
setNames :: [String] -> Model -> Model
setNames l m = m { _names = Just l }

-- |Returns one row of the `Model`.
row :: Int -> Model -> Row
row n = (IM.! n) . _rows

-- |Number of rows of the `Model`.
size :: Model -> Int
size = _size

-- |Returns the names of the rows.
names :: Model -> Maybe [String]
names = _names
