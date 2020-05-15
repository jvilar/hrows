{-# LANGUAGE OverloadedStrings #-}
module Model.RowStore.Base (
                -- *Types
                RowStore(..)
                , UpdatePlan(..)
                , FieldInfo(..)
                , RowsChanged
                , FieldName
                , RowStoreName
                , Row
                , RowPos
                , SortDirection(..)
                   -- *Functions
                , addRowStore
                , getRowStore
                , getRowStoreIndex
                , changed
                , names
                , fnames
                , row
                , rows
                , nFields
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
                , fieldIndex
                , setNames
                , size
                , types
) where

import Data.IntMap(IntMap)
import qualified Data.IntMap as IM
import Data.List(findIndex, sort, sortOn)
import Data.Maybe(fromMaybe, isJust)
import Data.Ord(Down(..))
import Data.Text(Text)
import qualified Data.Text as T
import TextShow(TextShow(showt))


import Model.Expression
import Model.Row
import Model.RowStoreConf

-- |The position of a `Row`.
type RowPos = Int

-- |Whether the store changed.
type RowsChanged = Bool

-- |The name of a `Field`
type FieldName = Text

-- |The name of a `RowStore`
type RowStoreName = Text

-- |Holds the rows.
data RowStore = RowStore { _nameRS :: RowStoreName
                         , _rows :: IntMap Row
                         , _dataSources :: [DataSource]
                         , _rowStores :: [RowStore]
                         , _fieldInfo :: [FieldInfo]
                         , _updatePlan :: UpdatePlan
                         , _nFields :: Int
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

-- |Determines the order in which updates must take place when a
-- `Field` changes
data UpdatePlan = UpdatePlan { expressions :: [Maybe Expression]
                             , influences :: IntMap [FieldPos]
                             , updateOrder :: [FieldPos]
                             , cycled :: [FieldPos]
                             } deriving Show

-- |Adds a `RowStore` to the store
addRowStore :: RowStore -> RowStore -> RowStore
addRowStore r rst = rst {
    _dataSources = rows r : _dataSources rst
    , _rowStores = r : _rowStores rst
}

-- |Recovers a `RowStore` from the store
getRowStore :: RowStore -> Int -> RowStore
getRowStore = (!!) . _rowStores

-- |Finds the position of the `RowStore` with the given name
getRowStoreIndex :: RowStore -> Text -> Maybe Int
getRowStoreIndex rst name = findIndex ((== name) . _nameRS) $ _rowStores rst

-- |Whether the store has changed.
changed :: RowStore -> RowsChanged
changed = _changed

-- |Find the first field with the given name.
fieldIndex :: RowStore -> FieldName -> Maybe Int
fieldIndex rst n = findIndex ((== Just n) . _name) $ _fieldInfo rst

-- |The types of the fields.
types :: RowStore -> [FieldType]
types = map _type . _fieldInfo

-- |Sets the names of the fields.
setNames :: [FieldName] -> RowStore -> RowStore
setNames l rst = rst { _fieldInfo = zipWith (\i n -> i { _name = Just n }) (_fieldInfo rst) l
                     , _changed = True
                     }

-- |Number of rows of the `RowStore`.
size :: RowStore -> Int
size = _size


-- |Transforms the `FieldInfo` into `FieldConf`.
toConf :: FieldInfo -> FieldConf
toConf inf = FieldConf { nameFC = _name inf
                       , typeFC = _type inf
                       , formulaFC = _formula inf
                       }


-- |Get the current configuration.
getConf :: RowStore -> RowStoreConf
getConf = RowStoreConf . map toConf . _fieldInfo


-- |Returns one row of the `RowStore`.
row :: RowPos -> RowStore -> Row
row n m | IM.null (_rows m) = emptyRow
        | otherwise = take (nFields m) $ fillEmpty (_rows m IM.! n)


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
nFields :: RowStore -> Int
nFields = _nFields

-- |Returns the names of the rows.
names :: RowStore -> Maybe [FieldName]
names = mapM _name . _fieldInfo

-- |Returns the names of the fields of the store with a default format.
fnames :: RowStore -> [FieldName]
fnames store = fromMaybe
                   (map (T.append "Campo " . showt) [1 .. nFields store])
                   (names store)

-- |Returns the rows of the `RowStore`.
rows :: RowStore -> DataSource
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

