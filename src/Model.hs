{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Model (
              -- *Types
              Model
             , Row
             , Field
             , RowPos
             , ColPos
             -- *Classes
             , ToField(..)
             -- *Functions
             -- **Construction
             , emptyRow
             , empty
             , addRow
             , setNames
             , fromRows
             -- **Querying
             , names
             , row
             , rows
             , size
             , sourceInfo
             , toString
             -- **Updating
             , changeField
             , setSourceInfo
) where

import Data.IntMap.Strict(IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List(foldl')

import SourceInfo

-- |A field can store an Int, a Double or a String or it may be empty.
data Field = AInt Int
           | ADouble Double
           | AString String
           | Empty
             deriving Show

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

-- |An empty 'Row'
emptyRow :: Row
emptyRow = []

-- |The position of a `Row`.
type RowPos = Int

-- |The position of a `Column`.
type ColPos = Int

-- |Holds the rows.
data Model = Model { _rows :: IntMap Row
                   , _names :: Maybe [String]
                   , _size :: Int
                   , _sourceInfo :: SourceInfo
                   }


-- |An empty `Model`.
empty :: Model
empty = Model { _rows = IM.empty
              , _names = Nothing
              , _size = 0
              , _sourceInfo = toSourceInfo ()
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

-- |Set the associated information.
setSourceInfo :: SourceInfoClass si => si -> Model -> Model
setSourceInfo si m = m { _sourceInfo = toSourceInfo si }

-- |Get the associated information
sourceInfo :: Model -> SourceInfo
sourceInfo = _sourceInfo

-- |Returns one row of the `Model`.
row :: RowPos -> Model -> Row
row n m | IM.null (_rows m) = emptyRow
        | otherwise = (IM.! n) $ _rows m

-- |Number of rows of the `Model`.
size :: Model -> Int
size = _size

-- |Returns the names of the rows.
names :: Model -> Maybe [String]
names = _names

-- |Returns the rows of the model.
rows :: Model -> [Row]
rows = IM.elems . _rows

-- |Changes one field.
changeField :: RowPos -> ColPos -> Field -> Model -> Model
changeField r c field m = m { _rows = IM.adjust (adjustCol c field) r (_rows m) }

adjustCol :: ColPos -> Field -> Row -> Row
adjustCol 0 x (_:xs) = x:xs
adjustCol n x (y:ys) = y : adjustCol (n-1) x ys
