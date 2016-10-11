module Model (
              -- *Types
              Model
             , Row
             , RowPos
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
             , nfields
             , size
             , formulas
             , types
             , isFormula
             , fieldFormula
             , fieldType
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
import Model.UpdatePlan

-- |The position of a `Row`.
type RowPos = Int

-- |Holds the rows.
data Model = Model { _rows :: IntMap Row
                   , _rowInfo :: [RowInfo]
                   , _updatePlan :: UpdatePlan
                   , _nfields :: Int
                   , _size :: Int
                   } deriving Show

-- |The information about a Row
data RowInfo = RowInfo { _name :: Maybe String
                       , _type :: FieldType
                       , _defaultValue :: Field
                       , _expression :: Maybe Expression
                       , _formula :: Maybe Formula
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
              , _updatePlan = mkUpdatePlan []
              , _nfields = 0
              , _size = 0
              }

fillEmpty :: Row -> Row
fillEmpty = (++ repeat (toField()))

-- |Adds a `Row` to a `Model`.
addRow :: Model -> Row -> Model
addRow m r = let
               r' = updateAll (_updatePlan m) $ zipWith convert (types m) (fillEmpty r)
             in m { _rows = IM.insert (_size m) r' (_rows m)
                  , _size = _size m + 1
                  }

-- |Adds an empty `Row` to a `Model`.
addEmptyRow :: Model -> Model
addEmptyRow m = addRow m (map _defaultValue $ _rowInfo m)

-- |Deletes a 'Row' from a 'Model'.
deleteRow :: RowPos -> Model -> Model
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
    m = addPlan $ empty { _nfields = length infos
                        , _rowInfo = infos
                        }
    in foldl' addRow m rs

-- |Sets the names of the fields.
setNames :: [String] -> Model -> Model
setNames l m = m { _rowInfo = zipWith (\i n -> i { _name = Just n }) (_rowInfo m) l }

-- |Returns one row of the `Model`.
row :: RowPos -> Model -> Row
row n m | IM.null (_rows m) = emptyRow
        | otherwise = take (nfields m) $ fillEmpty (_rows m IM.! n)

-- |Number of rows of the `Model`.
size :: Model -> Int
size = _size

-- |The formulas of the fields
formulas :: Model -> [Maybe Formula]
formulas = map _formula . _rowInfo

-- |True if the field is a formula
isFormula :: FieldPos -> Model -> Bool
isFormula c = isJust . _formula . (!! c) .  _rowInfo

-- |The formula of a field
fieldFormula :: FieldPos -> Model -> Maybe Formula
fieldFormula c = (!! c) . formulas

-- |The type of a field
fieldType :: FieldPos -> Model -> FieldType
fieldType c = (!! c) . types

-- |Number of fields of each row.
nfields :: Model -> Int
nfields = _nfields

-- |Returns the names of the rows.
names :: Model -> Maybe [String]
names = mapM _name . _rowInfo

-- |Returns the names of the model with a default format.
fnames :: Model -> [String]
fnames model = fromMaybe
                   (map (("Campo " ++).show) [1 .. nfields model])
                   (names model)

-- |Returns the rows of the model.
rows :: Model -> [Row]
rows = IM.elems . _rows

-- |Changes one field. Returns the new model and the fields changed.
changeField :: RowPos -> FieldPos -> Field -> Model -> (Model, [FieldPos])
changeField r c field m = let
      row = _rows m IM.! r
      field' = convert (_type $ _rowInfo m !! c) field
      (row', poss) = updateField (_updatePlan m) field' c row
    in if row !! c /= field'
       then (m { _rows = IM.insert r row' (_rows m) }, poss)
       else (m, [])

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
                        } | (n, t) <- zip (drop (_nfields m) names) (map snd l) ]
    rinfo = oldInfo ++ newInfo
    in addPlan $ m { _rows = IM.map (++ map (defaultValue . snd) l) (_rows m)
                   , _rowInfo = rinfo
                   , _nfields = _nfields m + length l
                   }

addPlan :: Model -> Model
addPlan m = let
    exps = map mParse $ _rowInfo m
    mParse ri = eliminateNames (fnames m) . addCast (_type ri) . parse <$> _formula ri
    up = mkUpdatePlan exps
  in m { _updatePlan = up, _rows = IM.map (updateAll up) (_rows m) }

adjustNames :: Model -> [Maybe String] -> [Maybe String]
adjustNames m newNames = zipWith (flip maybe Just . Just) defNames (map _name (_rowInfo m) ++ newNames)
    where defNames = ["Campo " ++ show n | n <- [(1 :: Int) ..]]

-- |Deletes the given fields from the model.
deleteFields :: [Int] -> Model -> Model
deleteFields fs m = addPlan $ m { _rows = IM.map (del fs) (_rows m)
                                , _rowInfo = del fs $ _rowInfo m
                                , _nfields = _nfields m - length fs
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
changeFieldType :: FieldType -> FieldPos -> Model -> Model
changeFieldType t n m | t /= types m !! n =
                          addPlan $ m { _rows = IM.map (change $ convert t) (_rows m)
                                      , _rowInfo = newInfo
                                      }
                      | otherwise = m
    where newInfo = change (\i -> i { _type = t, _defaultValue = defaultValue t }) $ _rowInfo m
          change f xs = let
              (h, x:t) = splitAt n xs
              in h ++ f x : t

-- |Changes the formula of the field.
changeFieldFormula :: Maybe Formula -> FieldPos -> Model -> Model
changeFieldFormula mf n m = addPlan $ m { _rowInfo = newInfo }
    where newInfo = change (\i -> i { _expression = c, _formula = mf }) $ _rowInfo m
          c = addCast (types m !! n) . eliminateNames (fnames m) . parse <$> mf
          change f xs = let
              (h, x:t) = splitAt n xs
              in h ++ f x : t
