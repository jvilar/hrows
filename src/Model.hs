module Model (
              -- *Types
              Model
             , Row
             , RowPos
             , ColPos
             -- *Functions
             -- **Construction
             , emptyRow
             , empty
             , addRow
             , addEmptyRow
             , deleteRow
             , setNames
             , fromRows
             -- **Querying
             , names
             , fnames
             , row
             , rows
             , ncols
             , size
             -- **Updating
             , changeField
             , newFields
             -- *Rexported
             , module Field
) where

import Data.IntMap.Strict(IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List(foldl')
import Data.Maybe(fromMaybe, isJust)

import Field

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
                   , _ncols :: Int
                   , _size :: Int
                   , _types :: [FieldType]
                   } deriving Show


-- |An empty `Model`.
empty :: Model
empty = Model { _rows = IM.empty
              , _names = Nothing
              , _ncols = 0
              , _size = 0
              , _types = []
              }

fillEmpty :: Row -> Row
fillEmpty = (++ repeat (toField()))

-- |Adds a `Row` to a `Model`.
addRow :: Model -> Row -> Model
addRow m r = let
               r' = zipWith convert (fillEmpty r) (_types m)
             in m { _rows = IM.insert (_size m) r' (_rows m)
                  , _size = _size m + 1
                  }

-- |Adds an empty `Row` to a `Model`.
addEmptyRow :: Model -> Model
addEmptyRow m = addRow m (map defaultValue $ _types m)

-- |Deletes a 'Row' from a 'Model'.
deleteRow :: Int -> Model -> Model
deleteRow pos m = m { _rows = IM.mapKeys f (_rows m)
                    , _size = _size m - 1
                    }
                  where f n | n <= pos = n
                            | otherwise = n - 1

-- |Creates a model from a list of `Row`s.
fromRows :: [Row] -> Model
fromRows rs = let
    types = foldl' combine [] rs
    combine xs row = xs ++ drop (length xs) (map typeOf row)
    m = empty { _ncols = length types
              , _types = types
              }
    in foldl' addRow m rs

-- |Sets the names of the field.
setNames :: [String] -> Model -> Model
setNames l m = m { _names = Just l }

-- |Returns one row of the `Model`.
row :: RowPos -> Model -> Row
row n m | IM.null (_rows m) = emptyRow
        | otherwise = take (ncols m) $ fillEmpty (_rows m IM.! n)

-- |Number of rows of the `Model`.
size :: Model -> Int
size = _size

-- |Number of columns of each row.
ncols :: Model -> Int
ncols = _ncols

-- |Returns the names of the rows.
names :: Model -> Maybe [String]
names = _names

-- |Returns the names of the model with a default format.
fnames :: Model -> [String]
fnames model = fromMaybe
                   (map (("Campo " ++).show) [1 .. ncols model])
                   (names model)

-- |Returns the rows of the model.
rows :: Model -> [Row]
rows = IM.elems . _rows

-- |Changes one field.
changeField :: RowPos -> ColPos -> Field -> Model -> Model
changeField r c field m = let
    field' = convert field $ _types m !! c
    in m { _rows = IM.adjust (adjustCol c field') r (_rows m) }

adjustCol :: ColPos -> Field -> Row -> Row
adjustCol 0 x (_:xs) = x:xs
adjustCol n x [] = replicate n (toField ()) ++ [x]
adjustCol n x (y:ys) = y : adjustCol (n-1) x ys

-- |Adds new fields to the model.
newFields :: [(Maybe String, FieldType)] -> Model -> Model
newFields l m = m { _names = adjustNames m (map fst l)
                  , _rows = IM.map (++ map (defaultValue . snd) l) (_rows m)
                  , _ncols = _ncols m + length l
                  , _types = _types m ++ map snd l
                  }

adjustNames :: Model -> [Maybe String] -> Maybe [String]
adjustNames m newNames
    | any isJust newNames || isJust (names m) = Just $ fnames m ++ newNames'
    | otherwise = Nothing
    where newNames' = zipWith combine [ncols m + 1 ..] newNames
          combine n = fromMaybe ("Campo " ++ show n)
