{-# LANGUAGE OverloadedStrings #-}

module Model.RowStore (
              -- *Types
              RowStore
             , RowsChanged
             , FieldName
             , Row
             , RowPos
             , SortDirection(..)
             -- *Functions
             -- **Construction
             , emptyRow
             , empty
             , emptyConf
             , addRow
             , addEmptyRow
             , deleteRow
             , fromRows
             , fromRowsNames
             , fromRowsConf
             -- **Querying
             , changed
             , names
             , fnames
             , row
             , rows
             , nfields
             , size
             , getConf
             , formulas
             , types
             , isFormula
             , fieldFormula
             , fieldType
             , fieldValues
             , nextPos
             -- **Updating
             , sortRows
             , setUnchanged
             , changeField
             , newFields
             , deleteFields
             , renameFields
             , importFields
             , importRows
             , moveField
             , changeFieldType
             , changeFieldFormula
             -- *Rexported
             , module Model.Field
             , module Model.Expression
) where

import Data.IntMap.Strict(IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List(foldl', sort, sortOn)
import Data.Map(Map)
import qualified Data.Map as M
import Data.Maybe(catMaybes, fromJust, fromMaybe, isJust, isNothing)
import Data.Ord(Down(..))
import Data.Text(Text)
import qualified Data.Text as T
import TextShow(TextShow(showt))

import Model.Expression
import Model.Field
import Model.ModelConf
import Model.Parser
import Model.Row
import Model.UpdatePlan

-- |The position of a `Row`.
type RowPos = Int

-- |Whether the store changed.
type RowsChanged = Bool

-- |The name of a Field
type FieldName = Text

-- |Holds the rows.
data RowStore = RowStore { _rows :: IntMap Row
                         , _fieldInfo :: [FieldInfo]
                         , _updatePlan :: UpdatePlan
                         , _nfields :: Int
                         , _size :: Int
                         , _changed :: RowsChanged
                         } deriving Show

-- |The information about a `Field`
data FieldInfo = FieldInfo { _name :: Maybe FieldName
                           , _type :: FieldType
                           , _defaultValue :: Field
                           , _expression :: Maybe Expression
                           , _formula :: Maybe Formula
                           } deriving Show

-- |Transforms the `FieldConf` into `FieldInfo`.
fromConf :: FieldConf -> FieldInfo
fromConf cnf = FieldInfo { _name = nameFC cnf
                         , _type = typeFC cnf
                         , _defaultValue = defaultValue $ typeFC cnf
                         , _expression = parse <$> formulaFC cnf
                         , _formula = formulaFC cnf
                         }

-- |Transforms the `FieldInfo` into `FieldConf`.
toConf :: FieldInfo -> FieldConf
toConf inf = FieldConf { nameFC = _name inf
                       , typeFC = _type inf
                       , formulaFC = _formula inf
                       }

inferInfo :: Field -> FieldInfo
inferInfo f = FieldInfo { _name = Nothing
                        , _type = typeOf f
                        , _defaultValue = defaultValue $ typeOf f
                        , _expression = Nothing
                        , _formula = Nothing
                        }

-- |The types of the fields.
types :: RowStore -> [FieldType]
types = map _type . _fieldInfo

-- |Whether the store has changed.
changed :: RowStore -> RowsChanged
changed = _changed

-- |An empty `RowStore`.
empty :: RowStore
empty = RowStore { _rows = IM.empty
              , _fieldInfo = []
              , _updatePlan = mkUpdatePlan []
              , _nfields = 0
              , _size = 0
              , _changed = False
              }

-- |Get the current configuration.
getConf :: RowStore -> ModelConf
getConf = ModelConf . map toConf . _fieldInfo

-- |An empty `RowStore` that has a configuration.
emptyConf :: ModelConf -> RowStore
emptyConf (ModelConf fcs) = addPlan RowStore { _rows = IM.empty
                                          , _fieldInfo = map fromConf fcs
                                          , _updatePlan = undefined
                                          , _nfields = length fcs
                                          , _size = 0
                                          , _changed = False
                                          }

fillEmpty :: Row -> Row
fillEmpty = (++ repeat (toField()))

-- |Adds a `Row` to a `RowStore`.
addRow :: RowStore -> Row -> RowStore
addRow m r = let
               r' = updateAll (_updatePlan m) $ zipWith convert (types m) (fillEmpty r)
             in m { _rows = IM.insert (_size m) r' (_rows m)
                  , _size = _size m + 1
                  , _changed = True
                  }

-- |Adds an empty `Row` to a `RowStore`.
addEmptyRow :: RowStore -> RowStore
addEmptyRow m = addRow m (map _defaultValue $ _fieldInfo m)

-- |Deletes a 'Row' from a 'RowStore'.
deleteRow :: RowPos -> RowStore -> RowStore
deleteRow pos m = m { _rows = IM.mapKeys f (_rows m)
                    , _size = _size m - 1
                    , _changed = True
                    }
                  where f n | n <= pos = n
                            | otherwise = n - 1

-- |Creates a `RowStore` from a list of `Row`s.
fromRows :: [Row] -> RowStore
fromRows rs = let
    infos = foldl' combine [] rs
    combine xs r = xs ++ map inferInfo (drop (length xs) r)
    m = addPlan empty { _nfields = length infos
                      , _fieldInfo = infos
                      }
    in (foldl' addRow m rs) { _changed = False }

-- |Creates a `RowStore` from a list of `Row`s and a list of names.
fromRowsNames :: [FieldName] -> [Row] -> RowStore
fromRowsNames l rs = (setNames l $ fromRows rs) { _changed = False }

-- |Creates a `RowStore` from a list of `Row`s and a `ModelConf`
fromRowsConf :: ModelConf -> [Row] -> RowStore
fromRowsConf conf  m = (foldl' addRow  (emptyConf conf) m) { _changed = False }

-- |Sets the names of the fields.
setNames :: [FieldName] -> RowStore -> RowStore
setNames l m = m { _fieldInfo = zipWith (\i n -> i { _name = Just n }) (_fieldInfo m) l
                 , _changed = True
                 }

-- |Returns one row of the `RowStore`.
row :: RowPos -> RowStore -> Row
row n m | IM.null (_rows m) = emptyRow
        | otherwise = take (nfields m) $ fillEmpty (_rows m IM.! n)

-- |Number of rows of the `RowStore`.
size :: RowStore -> Int
size = _size

-- |The formulas of the fields
formulas :: RowStore -> [Maybe Formula]
formulas = map _formula . _fieldInfo

-- |True if the field is a formula
isFormula :: FieldPos -> RowStore -> Bool
isFormula c = isJust . _formula . (!!! c) .  _fieldInfo

-- |The formula of a field
fieldFormula :: FieldPos -> RowStore -> Maybe Formula
fieldFormula c = (!!!  c) . formulas

-- |The type of a field
fieldType :: FieldPos -> RowStore -> FieldType
fieldType f = (!!! f) . types

-- |The values of a field
fieldValues :: FieldPos -> RowStore -> [Text]
fieldValues f = unique . sort . IM.foldr (\r l -> toString (r !!! f) : l) [] . _rows
                where unique [] = []
                      unique [x] = [x]
                      unique (x:r@(y:_)) | x == y = unique r
                                         | otherwise = x : unique r

-- |The position of the next appearance of a value
nextPos :: FieldPos -> Text -> RowPos -> RowStore -> RowPos
nextPos fpos s pos store = let
    v = convert (fieldType fpos store) $ toField s
    step i n (p, q) | n !!! fpos /= v = (p, q)
                    | i < pos = (Just $ maybe i (min i) p, q)
                    | i > pos = (p, Just $ maybe i (min i) q)
                    | otherwise = (p, q)
    (p, q) = IM.foldrWithKey step (Nothing, Nothing) $ _rows store
    in fromMaybe (fromMaybe pos p) q

-- |Number of fields of each row.
nfields :: RowStore -> Int
nfields = _nfields

-- |Returns the names of the rows.
names :: RowStore -> Maybe [FieldName]
names = mapM _name . _fieldInfo

-- |Returns the names of the fields of the store with a default format.
fnames :: RowStore -> [FieldName]
fnames store = fromMaybe
                   (map (T.append "Campo " . showt) [1 .. nfields store])
                   (names store)

-- |Returns the rows of the `RowStore`.
rows :: RowStore -> [Row]
rows = IM.elems . _rows

-- |The direction of a sort
data SortDirection = Ascending | Descending deriving Show

-- |Sort the rows of the store according to the field
sortRows :: FieldPos -> SortDirection -> RowStore -> RowStore
sortRows fp dir rst = rst { _changed = True
                          , _rows = rows'
                          }
                      where rs = IM.elems $ _rows rst
                            sorted = case dir of
                                        Ascending -> sortOn (!!! fp) rs
                                        Descending -> sortOn (Down . (!!! fp)) rs
                            rows' = IM.fromAscList $ zip [0..] sorted

-- |Marks the store as unchanged
setUnchanged :: RowStore -> RowStore
setUnchanged store = store { _changed = False }

-- |Changes one field. Returns the new store and the fields changed.
changeField :: RowPos -> FieldPos -> Field -> RowStore -> (RowStore, [FieldPos])
changeField r c field rst = let
      row = _rows rst IM.! r
      field' = convert (_type $ _fieldInfo rst !!! c) field
      (row', poss) = updateField (_updatePlan rst) field' c row
    in if row !!! c /= field'
       then (rst { _rows = IM.insert r row' (_rows rst), _changed = True }, poss)
       else (rst, [])

-- |Adds new fields to the store.
newFields :: [(Maybe FieldName, FieldType)] -> RowStore -> RowStore
newFields l rst = let
    names = adjustNames rst (map fst l)
    oldInfo = [ i { _name = n } | (i, n) <- zip (_fieldInfo rst) names ]
    newInfo = [ FieldInfo { _name = n
                        , _type = t
                        , _defaultValue = defaultValue t
                        , _expression = Nothing
                        , _formula = Nothing
                        } | (n, t) <- zip (drop (_nfields rst) names) (map snd l) ]
    rinfo = oldInfo ++ newInfo
    in addPlan rst { _rows = IM.map (++ map (defaultValue . snd) l) (_rows rst)
                 , _fieldInfo = rinfo
                 , _nfields = _nfields rst + length l
                 }

addPlan :: RowStore -> RowStore
addPlan m = let
    exps = map prepare $ _fieldInfo m
    prepare fi = eliminateNames (fnames m) . addCast (_type fi) <$> _expression fi
    up = mkUpdatePlan exps
  in m { _updatePlan = up, _rows = IM.map (updateAll up) (_rows m), _changed = True }

adjustNames :: RowStore -> [Maybe FieldName] -> [Maybe FieldName]
adjustNames m newNames = zipWith (flip maybe Just . Just) defNames (map _name (_fieldInfo m) ++ newNames)
    where defNames = ["Campo " `T.append` showt n | n <- [(1 :: Int) ..]]

-- |Deletes the given fields from the store.
deleteFields :: [FieldPos] -> RowStore -> RowStore
deleteFields fs rst = addPlan rst { _rows = IM.map (del fs) (_rows rst)
                                  , _fieldInfo = del fs $ _fieldInfo rst
                                  , _nfields = _nfields rst - length fs
                                  }

del :: [FieldPos] -> [a] -> [a]
del [] l = l
del pos l = go ps l
    where go [] l = l
          go (n:ns) l = let
              (i, _:t) = splitAt n l
              in i ++ go ns t
          spos = sort $ filter (< length l) $ map fromIntegral pos
          ps = head spos : zipWith (\n m -> n - m - 1) (tail spos) spos

-- |Changes the names of the fields to those given.
renameFields :: [FieldName] -> RowStore -> RowStore
renameFields names rst = addPlan rst { _fieldInfo = zipWith updateFInfo (_fieldInfo rst) names }
    where translations = catMaybes $ zipWith (\n1 n2 -> (,) <$> n1 <*> Just n2)
                                             (map _name $ _fieldInfo rst)
                                             names
          updateFInfo fi n = let
                               e = _expression fi
                               (e', changed) = translateNames translations $ fromJust e
                             in if isNothing e || not changed
                                then fi { _name = Just n }
                                else fi { _name = Just n
                                        , _expression = Just e'
                                        , _formula = Just $ toFormula e'
                                        }

-- |Imports fields from another store.
importFields :: RowStore -> [(FieldPos, FieldPos)] -> [(FieldPos, FieldPos)] -> RowStore -> RowStore
importFields other keys values rst = addPlan rst { _rows = IM.map importRow (_rows rst) }
    where importRow r = case findRow r of
                            Nothing -> r
                            Just vals' -> replace r vals'
              where findRow r = M.lookup (key r) keyTable
                    key r = map ((r !!!) . fst) keys
                    keyTable = prepareKeyTable keys values (IM.elems $ _rows other)

-- |Imports rows from another store.
importRows :: RowStore -> [(FieldPos, FieldPos)] -> RowStore -> RowStore
importRows other values rst = addPlan rst {
  _rows = IM.union (_rows rst) (IM.mapKeys (+ size rst) $ IM.map changeValues $ _rows other),
  _size = _size rst + _size other
  }
    where changeValues = replace myEmpty . valuesOf values
          myEmpty = map _defaultValue $ _fieldInfo rst

-- |Returns a table that associates to each valid combination of
-- fields the corresponding value.
prepareKeyTable :: [(FieldPos, FieldPos)] -> [(FieldPos, FieldPos)] -> [Row] -> Map [Field] [(FieldPos, Field)]
prepareKeyTable keys values other = M.fromList asig
    where asig = [(key r, valuesOf values r) | r <- other]
          key r = map ((r !!!) . snd) keys

-- Returns the values associated to a position from the corresponding
-- position in the row, given the association of input positions to
-- output positions
valuesOf :: [(FieldPos, FieldPos)] -> Row -> [(FieldPos, Field)]
valuesOf values r = sort [(i, r !!! o) | (i, o) <- values]

-- Replaces the fields in the given positions.
replace :: Row -> [(FieldPos, Field)] -> Row
replace = go 0
  where go _ [] _ = []
        go _ l [] = l
        go n (f:fs) ((pos, val):rest) | n /= pos = f : go (n+1) fs ((pos, val):rest)
                                      | otherwise = convert (typeOf f) val : go (n+1) fs rest



-- |Move a field to the position just next to the other
moveField :: FieldPos -> FieldPos -> RowStore -> RowStore
moveField from to rst = let
    from' = fromIntegral from
    to' = fromIntegral to
    perm = case compare from to of
               EQ -> id
               LT -> mvf from' to'
               GT -> mvb from' to'
    iperm = case compare from to of
               EQ -> id
               LT -> mvb to' from'
               GT -> mvf to' from'
    newPos = iperm [0 .. _nfields rst - 1]
    updateFInfo fi = let
                       e = _expression fi
                       (e', changed) = translatePositions newPos $ fromJust e
                     in if isNothing e || not changed
                        then fi
                        else fi { _expression = Just e'
                                , _formula = Just $ toFormula e'
                                }

    in if from == to
       then rst
       else addPlan rst { _rows = IM.map perm (_rows rst)
                      , _fieldInfo = perm . map updateFInfo $ _fieldInfo rst
                      }
    where
      -- move forward, ie from < to
      mvf from to l = let
          (left, f:right) = splitAt from l
          (before, after) = splitAt (to - from) right
        in left ++ before ++ f:after
      -- move backward, ie from > to
      mvb from to l = let
          (left, right) = splitAt to l
          (before, f: after) = splitAt (from - to) right
        in left ++ f:before ++ after

-- |Changes the type of the field.
changeFieldType :: FieldType -> FieldPos -> RowStore -> RowStore
changeFieldType t n rst | t /= types rst !! n' =
                            addPlan rst { _rows = IM.map (change $ convert t) (_rows rst)
                                        , _fieldInfo = newInfo
                                        }
                        | otherwise = rst
    where newInfo = change (\i -> i { _type = t, _defaultValue = defaultValue t }) $ _fieldInfo rst
          change f xs = let
              (h, x:t) = splitAt n' xs
              in h ++ f x : t
          n' = fromIntegral n

-- |Changes the formula of the field.
changeFieldFormula :: Maybe Formula -> FieldPos -> RowStore -> RowStore
changeFieldFormula mf n rst = addPlan rst { _fieldInfo = newInfo, _changed = True }
    where newInfo = change (\fi -> fi { _expression = parse <$> mf, _formula = mf }) $ _fieldInfo rst
          change f xs = let
              (h, x:t) = splitAt (fromIntegral n) xs
              in h ++ f x : t
