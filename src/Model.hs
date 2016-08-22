{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Model (
              -- *Types
              Model
             -- *Constants
             , model0
             -- *Functions
             , row
             , size
) where

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

-- |A row is a list of fields.
type Row = [Field]

-- |Holds the rows.
data Model = Model { _rows :: [Row]
                   , _names :: Maybe [String]
                   , _size :: Int
                   }

-- |The initial model for tests.
model0 :: Model
model0 = Model { _rows = [toField <$> ["one", "two", "three"]
                         ,toField <$> ["uno", "dos", "tres"]
                         ,toField <$> ["eins", "zwei", "drei"]
                         ]
               , _names = Just ["Col1", "Col2", "Col3"]
               , _size = 3
               }

-- |Returns one row of the `Model`.
row :: Int -> Model -> Row
row n = (!!n) . _rows

-- |Number of rows of the `Model`.
size :: Model -> Int
size = _size
