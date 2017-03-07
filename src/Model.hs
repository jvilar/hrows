module Model (
              -- *Types
              Model
             , ModelChanged
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
import Debug.Trace

import Model.Expression
import Model.Field
import Model.ModelConf
import Model.Parser
import Model.Row
import Model.UpdatePlan

-- |The position of a `Row`.
type RowPos = Int

-- |Whether the model changed.
type ModelChanged = Bool

-- |Holds the rows.
data Model = Model { _rows :: IntMap Row
                   , _fieldInfo :: [FieldInfo]
                   , _updatePlan :: UpdatePlan
                   , _nfields :: Int
                   , _size :: Int
                   , _changed :: ModelChanged
                   } deriving Show

-- |The information about a Field
data FieldInfo = FieldInfo { _name :: Maybe String
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
types :: Model -> [FieldType]
types = map _type . _fieldInfo

-- |Whether the model has changed.
changed :: Model -> ModelChanged
changed = _changed

-- |An empty `Model`.
empty :: Model
empty = Model { _rows = IM.empty
              , _fieldInfo = []
              , _updatePlan = mkUpdatePlan []
              , _nfields = 0
              , _size = 0
              , _changed = False
              }

-- |Get the current configuration.
getConf :: Model -> ModelConf
getConf = ModelConf . map toConf . _fieldInfo

-- |An empty `Model` that has a configuration.
emptyConf :: ModelConf -> Model
emptyConf (ModelConf fcs) = addPlan Model { _rows = IM.empty
                                          , _fieldInfo = map fromConf fcs
                                          , _updatePlan = undefined
                                          , _nfields = length fcs
                                          , _size = 0
                                          , _changed = False
                                          }

fillEmpty :: Row -> Row
fillEmpty = (++ repeat (toField()))

-- |Adds a `Row` to a `Model`.
addRow :: Model -> Row -> Model
addRow m r = let
               r' = updateAll (_updatePlan m) $ zipWith convert (types m) (fillEmpty r)
             in m { _rows = IM.insert (_size m) r' (_rows m)
                  , _size = _size m + 1
                  , _changed = True
                  }

-- |Adds an empty `Row` to a `Model`.
addEmptyRow :: Model -> Model
addEmptyRow m = addRow m (map _defaultValue $ _fieldInfo m)

-- |Deletes a 'Row' from a 'Model'.
deleteRow :: RowPos -> Model -> Model
deleteRow pos m = m { _rows = IM.mapKeys f (_rows m)
                    , _size = _size m - 1
                    , _changed = True
                    }
                  where f n | n <= pos = n
                            | otherwise = n - 1

-- |Creates a `Model` from a list of `Row`s.
fromRows :: [Row] -> Model
fromRows rs = let
    infos = foldl' combine [] rs
    combine xs r = xs ++ map inferInfo (drop (length xs) r)
    m = addPlan empty { _nfields = length infos
                      , _fieldInfo = infos
                      }
    in (foldl' addRow m rs) { _changed = False }

-- |Creates a `Model` from a list of `Row`s and a list of names.
fromRowsNames :: [String] -> [Row] -> Model
fromRowsNames l rs = (setNames l $ fromRows rs) { _changed = False }

-- |Creates a model from a list of `Row`s and a `ModelConf`
fromRowsConf :: ModelConf -> [Row] -> Model
fromRowsConf conf  m = (foldl' addRow  (emptyConf conf) m) { _changed = False }

-- |Sets the names of the fields.
setNames :: [String] -> Model -> Model
setNames l m = m { _fieldInfo = zipWith (\i n -> i { _name = Just n }) (_fieldInfo m) l
                 , _changed = True
                 }

-- |Returns one row of the `Model`.
row :: RowPos -> Model -> Row
row n m | IM.null (_rows m) = emptyRow
        | otherwise = take (nfields m) $ fillEmpty (_rows m IM.! n)

-- |Number of rows of the `Model`.
size :: Model -> Int
size = _size

-- |The formulas of the fields
formulas :: Model -> [Maybe Formula]
formulas = map _formula . _fieldInfo

-- |True if the field is a formula
isFormula :: FieldPos -> Model -> Bool
isFormula c = isJust . _formula . (!! c) .  _fieldInfo

-- |The formula of a field
fieldFormula :: FieldPos -> Model -> Maybe Formula
fieldFormula c = (!! c) . formulas

-- |The type of a field
fieldType :: FieldPos -> Model -> FieldType
fieldType f = (!! f) . types

-- |The values of a field
fieldValues :: FieldPos -> Model -> [String]
fieldValues f = unique . sort . IM.foldr (\r l -> toString (r !! f) : l) [] . _rows
                where unique [] = []
                      unique [x] = [x]
                      unique (x:r@(y:l)) | x == y = unique r
                                         | otherwise = x : unique r

-- |The position of the next appearance of a value
nextPos :: FieldPos -> String -> RowPos -> Model -> RowPos
nextPos fpos s pos model = let
    v = convert (fieldType fpos model) $ toField s
    step i n (p, q) | n !! fpos /= v = (p, q)
                    | i < pos = (Just $ maybe i (min i) p, q)
                    | i > pos = (p, Just $ maybe i (min i) q)
                    | otherwise = (p, q)
    (p, q) = IM.foldrWithKey step (Nothing, Nothing) $ _rows model
    in fromMaybe (fromMaybe pos p) q

-- |Number of fields of each row.
nfields :: Model -> Int
nfields = _nfields

-- |Returns the names of the rows.
names :: Model -> Maybe [String]
names = mapM _name . _fieldInfo

-- |Returns the names of the model with a default format.
fnames :: Model -> [String]
fnames model = fromMaybe
                   (map (("Campo " ++).show) [1 .. nfields model])
                   (names model)

-- |Returns the rows of the model.
rows :: Model -> [Row]
rows = IM.elems . _rows

-- |The direction of a sort
data SortDirection = Ascending | Descending deriving Show

-- |Sort the rows of the model according to the field
sortRows :: FieldPos -> SortDirection -> Model -> Model
sortRows fp dir m = m { _changed = True
                      , _rows = rows'
                      }
                    where rs = IM.elems $ _rows m
                          sorted = case dir of
                                      Ascending -> sortOn (!! fp) rs
                                      Descending -> sortOn (Down . (!! fp)) rs
                          rows' = IM.fromAscList $ zip [0..] sorted

-- |Marks the model as unchanged
setUnchanged :: Model -> Model
setUnchanged model = model { _changed = False }

-- |Changes one field. Returns the new model and the fields changed.
changeField :: RowPos -> FieldPos -> Field -> Model -> (Model, [FieldPos])
changeField r c field m = let
      row = _rows m IM.! r
      field' = convert (_type $ _fieldInfo m !! c) field
      (row', poss) = updateField (_updatePlan m) field' c row
    in if row !! c /= field'
       then (m { _rows = IM.insert r row' (_rows m), _changed = True }, poss)
       else (m, [])

-- |Adds new fields to the model.
newFields :: [(Maybe String, FieldType)] -> Model -> Model
newFields l m = let
    names = adjustNames m (map fst l)
    oldInfo = [ i { _name = n } | (i, n) <- zip (_fieldInfo m) names ]
    newInfo = [ FieldInfo { _name = n
                        , _type = t
                        , _defaultValue = defaultValue t
                        , _expression = Nothing
                        , _formula = Nothing
                        } | (n, t) <- zip (drop (_nfields m) names) (map snd l) ]
    rinfo = oldInfo ++ newInfo
    in addPlan m { _rows = IM.map (++ map (defaultValue . snd) l) (_rows m)
                 , _fieldInfo = rinfo
                 , _nfields = _nfields m + length l
                 }

addPlan :: Model -> Model
addPlan m = let
    exps = map prepare $ _fieldInfo m
    prepare fi = eliminateNames (fnames m) . addCast (_type fi) <$> _expression fi
    up = mkUpdatePlan exps
  in m { _updatePlan = up, _rows = IM.map (updateAll up) (_rows m), _changed = True }

adjustNames :: Model -> [Maybe String] -> [Maybe String]
adjustNames m newNames = zipWith (flip maybe Just . Just) defNames (map _name (_fieldInfo m) ++ newNames)
    where defNames = ["Campo " ++ show n | n <- [(1 :: Int) ..]]

-- |Deletes the given fields from the model.
deleteFields :: [FieldPos] -> Model -> Model
deleteFields fs m = addPlan m { _rows = IM.map (del fs) (_rows m)
                              , _fieldInfo = del fs $ _fieldInfo m
                              , _nfields = _nfields m - length fs
                              }

del :: [Int] -> [a] -> [a]
del [] l = l
del pos l = go ps l
    where go [] l = l
          go (n:ns) l = let
              (i, _:t) = splitAt n l
              in i ++ go ns t
          spos = sort $ filter (< length l) pos
          ps = head spos : zipWith (\n m -> n - m - 1) (tail spos) spos

-- |Changes the names of the fields to those given.
renameFields :: [String] -> Model -> Model
renameFields names m = addPlan m { _fieldInfo = zipWith updateFInfo (_fieldInfo m) names }
    where translations = catMaybes $ zipWith (\n1 n2 -> (,) <$> n1 <*> Just n2)
                                             (map _name $ _fieldInfo m)
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

-- |Imports fields from another model.
importFields :: Model -> [(FieldPos, FieldPos)] -> [(FieldPos, FieldPos)] -> Model -> Model
importFields other keys values m = addPlan m { _rows = IM.map importRow (_rows m) }
    where importRow r = case findRow r of
                            Nothing -> r
                            Just vals' -> replace r vals'
              where findRow r = M.lookup (key r) keyTable
                    key r = map (r !!) $ map fst keys
                    replace = go 0
                              where go _ [] _ = []
                                    go _ l [] = l
                                    go n (f:fs) ((pos, val):rest) | n /= pos = f : go (n+1) fs ((pos, val):rest)
                                                                  | otherwise = val : go (n+1) fs rest
                    keyTable = prepareKeyTable keys values (IM.elems $ _rows other)

-- |Returns a table that associates to each valid combination of
-- fields the corresponding value.
prepareKeyTable :: [(FieldPos, FieldPos)] -> [(FieldPos, FieldPos)] -> [Row] -> Map [Field] [(FieldPos, Field)]
prepareKeyTable keys values other = M.fromList asig
    where asig = [(key r, valuesOf r) | r <- other]
          key r = map (r !!) $ map snd keys
          valuesOf r = sort [(i, r !! o) | (i, o) <- values]

-- |Move a field to the position just next to the other
moveField :: FieldPos -> FieldPos -> Model -> Model
moveField from to m = let
    perm = case compare from to of
               EQ -> id
               LT -> mvf from to
               GT -> mvb from to
    iperm = case compare from to of
               EQ -> id
               LT -> mvb to from
               GT -> mvf to from
    newPos = iperm [0 .. _nfields m - 1]
    updateFInfo fi = let
                       e = _expression fi
                       (e', changed) = translatePositions newPos $ fromJust e
                     in if isNothing e || not changed
                        then fi
                        else fi { _expression = Just e'
                                , _formula = Just $ toFormula e'
                                }

    in if from == to
       then m
       else addPlan m { _rows = IM.map perm (_rows m)
                      , _fieldInfo = perm . map updateFInfo $ _fieldInfo m
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
changeFieldType :: FieldType -> FieldPos -> Model -> Model
changeFieldType t n m | t /= types m !! n =
                          addPlan m { _rows = IM.map (change $ convert t) (_rows m)
                                    , _fieldInfo = newInfo
                                    }
                      | otherwise = m
    where newInfo = change (\i -> i { _type = t, _defaultValue = defaultValue t }) $ _fieldInfo m
          change f xs = let
              (h, x:t) = splitAt n xs
              in h ++ f x : t

-- |Changes the formula of the field.
changeFieldFormula :: Maybe Formula -> FieldPos -> Model -> Model
changeFieldFormula mf n m = addPlan m { _fieldInfo = newInfo, _changed = True }
    where newInfo = change (\fi -> fi { _expression = parse <$> mf, _formula = mf }) $ _fieldInfo m
          change f xs = let
              (h, x:t) = splitAt n xs
              in h ++ f x : t
