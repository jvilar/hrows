{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Model.RowStore.Update (
  updateAll
  , addRow
  , addRowStore
  , addEmptyRow
  , deleteRow
  , emptyConf
  , emptyName
  , fromRows
  , fromRowsNames
  , fromRowsConf
  , changeField
  , newFields
  , deleteFields
  , renameFields
  , changeVisibleFields
  , hideField
  , importFields
  , importRows
  , moveField
  , changeFieldType
  , changeFieldFormula
  , renameDataSources
  , deleteDataSources
) where

import qualified Data.IntMap.Strict as IM

import Data.List(sort, foldl')
import Data.Map(Map)
import qualified Data.Map as M
import Data.Maybe(catMaybes, fromJust, isNothing)
import qualified Data.Text as T
import TextShow(TextShow(showt))


import Model.Field
import Model.Expression
import Model.Expression.Evaluation
import Model.Expression.Manipulate
import Model.Expression.Parser
import Model.Row
import Model.RowStore.Base
import Model.RowStore.RowStoreConf
import Model.RowStore.UpdatePlan

updateAll :: UpdatePlan -> [DataSource] -> Row -> Row
updateAll up dss r = foldr (changeRow $ mkError "Fórmula con dependencias circulares")
                 (foldl' (evaluateField up dss) r (updateOrder up)) (cycled up)

-- |Changes a 'Field' and Updates a 'Row'. Returns the new 'Row'
-- and a list of the positions of the fields that changed, including
-- the one responsible of the change.
updateField :: UpdatePlan -> Field -> FieldPos -> [DataSource] -> Row -> (Row, [FieldPos])
updateField up f n dss r = let
    deps = influences up IM.! fromIntegral n
    in (foldl' (evaluateField up dss) (changeRow f n r) deps, n:deps)

changeRow :: Field -> FieldPos -> Row -> Row
changeRow f n r = let
    (h, _:t) = splitAt (fromIntegral n) r
    in h ++ f : t

evaluateField :: UpdatePlan -> [DataSource] -> Row -> FieldPos -> Row
evaluateField up dss r f = let
    v = evaluate r dss (fromJust $ expressions up !! fromIntegral f)
    in changeRow v f r

-- |Adds a `Row` to a `RowStore`.
addRow :: RowStore -> Row -> RowStore
addRow m r = let
               r' = updateAll (_updatePlan m) (_dataSources m) $ zipWith convertKeepText (types m) (fillEmpty r)
             in m { _rows = IM.insert (_size m) r' (_rows m)
                  , _size = _size m + 1
                  , _changed = True
                  }

-- |Adds an empty `Row` to a `RowStore`.
addEmptyRow :: RowStore -> RowStore
addEmptyRow m = addRow m (map _defaultValue $ _fieldInfo m)

-- |Deletes a 'Row' from a 'RowStore'.
deleteRow :: RowPos -> RowStore -> RowStore
deleteRow pos m@RowStore {..} = m { _rows = rows'
                                  , _size = _size - 1
                                  , _changed = True
                                  }
                  where f n | n <= pos = n
                            | otherwise = n - 1
                        rows' | pos == _size - 1 = IM.delete pos _rows
                              | otherwise = IM.mapKeys f _rows

-- |An empty `RowStore` that has a configuration.
emptyConf :: RowStoreName -> RowStoreConf -> RowStore
emptyConf n conf = addPlan (emptyName n) { _fieldInfo = map fromConf fcs
                                     , _nFields = length fcs
                                     }
                              where fcs = fieldConf conf

-- |Transforms the `FieldConf` into `FieldInfo`.
fromConf :: FieldConf -> FieldInfo
fromConf cnf = FieldInfo { _name = nameFC cnf
                         , _type = typeFC cnf
                         , _defaultValue = defaultValue $ typeFC cnf
                         , _expression = parseExpression <$> formulaFC cnf
                         , _formula = formulaFC cnf
                         , _visible = True
                         }

-- |An empty `RowStore` with a name.
emptyName :: RowStoreName -> RowStore
emptyName name = RowStore { _nameRS = name
                          , _rows = IM.empty
                          , _dataSources = []
                          , _rowStores = []
                          , _fieldInfo = []
                          , _updatePlan = mkUpdatePlan []
                          , _nFields = 0
                          , _size = 0
                          , _changed = False
                          }

-- |Creates a `RowStore` from a list of `Row`s.
fromRows :: RowStoreName -> [Row] -> RowStore
fromRows name rs = let
    infos = foldl' combine [] rs
    combine xs r = xs ++ map inferInfo (drop (length xs) r)
    rst = addPlan (emptyName name) { _nFields = length infos
                               , _fieldInfo = infos
                               }
    in (foldl' addRow rst rs) { _changed = False }

inferInfo :: Field -> FieldInfo
inferInfo f = FieldInfo { _name = Nothing
                        , _type = typeOf f
                        , _defaultValue = defaultValue $ typeOf f
                        , _expression = Nothing
                        , _formula = Nothing
                        , _visible = True
                        }

-- |Creates a `RowStore` from a list of `Row`s and a list of names.
fromRowsNames :: RowStoreName -> [FieldName] -> [Row] -> RowStore
fromRowsNames n l rs = (setNames l $ fromRows n rs) { _changed = False }

-- |Creates a `RowStore` from a list of `Row`s and a `RowStoreConf`
fromRowsConf :: RowStoreName -> RowStoreConf -> [Row] -> RowStore
fromRowsConf n conf rs = (foldl' addRow  (emptyConf n conf) rs) { _changed = False }

-- |Changes one field. Returns the new store and the fields changed.
changeField :: RowPos -> FieldPos -> Field -> RowStore -> (RowStore, [FieldPos])
changeField r c field rst = let
      row = _rows rst IM.! r
      field' = convertKeepText (_type $ _fieldInfo rst !!! c) field
      (row', poss) = updateField (_updatePlan rst) field' c (_dataSources rst) row
    in if row !!! c /= field'
       then (rst { _rows = IM.insert r row' (_rows rst), _changed = True }, poss)
       else (rst, [])

-- |Adds a `RowStore` to the store
addRowStore :: RowStore -> RowStore -> RowStore
addRowStore r rst = addPlan $ rst {
    _dataSources = rows r : _dataSources rst
    , _rowStores = r : _rowStores rst
  }

-- |Creates an `UpdatePlan` for the `RowStore` and recomputes all the rows. It is
-- used whenever a changed may potentially change the values of the formulas.
addPlan :: RowStore -> RowStore
addPlan rst = let
    exps = map prepare $ _fieldInfo rst
    prepare fi = addPositions rst . addCast (_type fi) <$> _expression fi
    up = mkUpdatePlan exps
  in rst { _updatePlan = up, _rows = IM.map (updateAll up $ _dataSources rst) (_rows rst), _changed = True }

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
                          , _visible = True
                          } | (n, t) <- zip (drop (_nFields rst) names) (map snd l) ]
    rinfo = oldInfo ++ newInfo
    in addPlan rst { _rows = IM.map (++ map (defaultValue . snd) l) (_rows rst)
                 , _fieldInfo = rinfo
                 , _nFields = _nFields rst + length l
                 }


adjustNames :: RowStore -> [Maybe FieldName] -> [Maybe FieldName]
adjustNames m newNames = zipWith (flip maybe Just . Just) defNames (map _name (_fieldInfo m) ++ newNames)
    where defNames = ["Campo " `T.append` showt n | n <- [(1 :: Int) ..]]

-- |Deletes the given fields from the store.
deleteFields :: [FieldPos] -> RowStore -> RowStore
deleteFields fs rst = addPlan rst { _rows = IM.map (del fs) (_rows rst)
                                  , _fieldInfo = del fs $ _fieldInfo rst
                                  , _nFields = _nFields rst - length fs
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

-- |Changes the visible property of the fields according to the list.
changeVisibleFields :: [Bool] -> RowStore -> RowStore
changeVisibleFields vs rst = rst { _fieldInfo = zipWith (\fi v -> fi { _visible = v })
                                                       (_fieldInfo rst) vs
                                 }

-- |Hides a single field
hideField :: FieldPos -> RowStore -> RowStore
hideField fpos rst = rst { _fieldInfo = zipWith f (_fieldInfo rst) [0..] }
    where f fi n | n == fpos = fi { _visible = False }
                 | otherwise = fi

-- Returns the values associated to a position from the corresponding
-- position in the row, given the association of input positions to
-- output positions
valuesOf :: [(FieldPos, FieldPos)] -> Row -> [(FieldPos, Field)]
valuesOf values r = sort [(i, r !!! o) | (i, o) <- values]


-- |Returns a table that associates to each valid combination of
-- fields the corresponding value.
prepareKeyTable :: [(FieldPos, FieldPos)] -> [(FieldPos, FieldPos)] -> [Row] -> Map [Field] [(FieldPos, Field)]
prepareKeyTable keys values other = M.fromList asig
    where asig = [(key r, valuesOf values r) | r <- other]
          key r = map ((r !!!) . snd) keys

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
    newPos = iperm [0 .. _nFields rst - 1]
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
    where newInfo = change (\fi -> fi { _expression = parseExpression <$> mf, _formula = mf }) $ _fieldInfo rst
          change f xs = let
              (h, x:t) = splitAt (fromIntegral n) xs
              in h ++ f x : t

-- |Change the names of the `DataSource` s
renameDataSources :: [RowStoreName] -> RowStore -> RowStore
renameDataSources nms rst = addPlan rst { _rowStores = zipWith setName nms $ _rowStores rst }

-- |Delete the sources with the given names
deleteDataSources :: [RowStoreName] -> RowStore -> RowStore
deleteDataSources nms rst = addPlan rst { _rowStores = filter ((`notElem` nms) . getName) $ _rowStores rst }
