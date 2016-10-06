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
             , formulas
             -- **Updating
             , changeField
             , newFields
             , deleteFields
             , changeFieldType
             , changeFieldFormula
             -- *Rexported
             , module Model.Field
             , module Model.Expression
) where

import Control.Arrow((&&&))
import Data.IntMap.Strict(IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List(foldl', sort)
import Data.Maybe(fromMaybe, isJust)

import Model.Expression
import Model.Field
import Model.Parser
import Model.Row


-- |The position of a `Row`.
type RowPos = Int

-- |The position of a `Column`.
type ColPos = Int

-- |Holds the rows.
data Model = Model { _rows :: IntMap Row
                   , _rowInfo :: [RowInfo]
                   , _ncols :: Int
                   , _size :: Int
                   } deriving Show

-- |The information about a Row
data RowInfo = RowInfo { _name :: Maybe String
                       , _type :: FieldType
                       , _defaultValue :: Field
                       , _expression :: Maybe Expression
                       , _formula :: Maybe String
                       } deriving Show

inferInfo :: Field -> RowInfo
inferInfo f = RowInfo { _name = Nothing
                      , _type = typeOf f
                      , _defaultValue = defaultValue $ typeOf f
                      , _expression = Nothing
                      , _formula = Nothing
                      }

types :: Model -> [FieldType]
types = map _type . _rowInfo

-- |An empty `Model`.
empty :: Model
empty = Model { _rows = IM.empty
              , _rowInfo = []
              , _ncols = 0
              , _size = 0
              }

fillEmpty :: Row -> Row
fillEmpty = (++ repeat (toField()))

-- |Adds a `Row` to a `Model`.
addRow :: Model -> Row -> Model
addRow m r = let
               r' = zipWith convert (types m) (fillEmpty r)
             in m { _rows = IM.insert (_size m) r' (_rows m)
                  , _size = _size m + 1
                  }

-- |Adds an empty `Row` to a `Model`.
addEmptyRow :: Model -> Model
addEmptyRow m = addRow m (map _defaultValue $ _rowInfo m)

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
    infos = foldl' combine [] rs
    combine xs r = xs ++ map inferInfo (drop (length xs) r)
    m = empty { _ncols = length infos
              , _rowInfo = infos
              }
    in foldl' addRow m rs

-- |Sets the names of the fields.
setNames :: [String] -> Model -> Model
setNames l m = m { _rowInfo = zipWith (\i n -> i { _name = Just n }) (_rowInfo m) l }

-- |Returns one row of the `Model`.
row :: RowPos -> Model -> Row
row n m | IM.null (_rows m) = emptyRow
        | otherwise = take (ncols m) $ fillEmpty (_rows m IM.! n)

-- |Number of rows of the `Model`.
size :: Model -> Int
size = _size

-- |The formulas of the fields
formulas :: Model -> [Maybe String]
formulas = map _formula . _rowInfo


-- |Number of columns of each row.
ncols :: Model -> Int
ncols = _ncols

-- |Returns the names of the rows.
names :: Model -> Maybe [String]
names = sequence . map _name . _rowInfo

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
    field' = convert (_type $ _rowInfo m !! c) field
    in m { _rows = IM.adjust (adjustCol c field') r (_rows m) }

adjustCol :: ColPos -> Field -> Row -> Row
adjustCol 0 x (_:xs) = x:xs
adjustCol n x [] = replicate n (toField ()) ++ [x]
adjustCol n x (y:ys) = y : adjustCol (n-1) x ys

-- |Adds new fields to the model.
newFields :: [(Maybe String, FieldType)] -> Model -> Model
newFields l m = let
    names = adjustNames m (map fst l)
    oldInfo = [ i { _name = n } | (i, n) <- zip (_rowInfo m) names ]
    newInfo = [ RowInfo { _name = n
                        , _type = t
                        , _defaultValue = defaultValue t
                        , _expression = Nothing
                        , _formula = Nothing
                        } | (n, t) <- zip (drop (_ncols m) names) (map snd l) ]
    in m { _rows = IM.map (++ map (defaultValue . snd) l) (_rows m)
         , _rowInfo = oldInfo ++ newInfo
         , _ncols = _ncols m + length l
         }

adjustNames :: Model -> [Maybe String] -> [Maybe String]
adjustNames m newNames = zipWith (flip maybe Just . Just) defNames ((map _name $ _rowInfo m) ++ newNames)
    where defNames = ["Campo " ++ show n | n <- [1 ..]]

-- |Deletes the given fields from the model.
deleteFields :: [Int] -> Model -> Model
deleteFields fs m = m { _rows = IM.map (del fs) (_rows m)
                      , _rowInfo = del fs $ _rowInfo m
                      , _ncols = _ncols m - length fs
                      }

del :: [Int] -> [a] -> [a]
del [] l = l
del pos l = go ps l
    where go [] l = l
          go (n:ps) l = let
              (i, _:t) = splitAt n l
              in i ++ go ps t
          spos = sort $ filter (< length l) pos
          ps = head spos : zipWith (\n m -> n - m - 1) (tail spos) spos

-- |Changes the type of the field.
changeFieldType :: FieldType -> Int -> Model -> Model
changeFieldType t n m | t /= types m !! n =
                          m { _rows = IM.map (change $ convert t) (_rows m)
                            , _rowInfo = newInfo
                            }
                      | otherwise = m
    where newInfo = change (\i -> i { _type = t, _defaultValue = defaultValue t }) $ _rowInfo m
          change f xs = let
              (h, x:t) = splitAt n xs
              in h ++ f x : t

-- |Changes the formula of the field.
changeFieldFormula :: Maybe String -> Int -> Model -> Model
changeFieldFormula mf n m = m { _rowInfo = newInfo
                              }
    where newInfo = change (\i -> i { _expression = c, _formula = mf }) $ _rowInfo m
          c = parse <$> mf
          change f xs = let
              (h, x:t) = splitAt n xs
              in h ++ f x : t
