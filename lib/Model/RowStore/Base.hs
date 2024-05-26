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
                , getRowStore
                , getRowStoreIndex
                , getRowStores
                , getDataSources
                , getName
                , changed
                , names
                , fnames
                , visibilities
                , row
                , rows
                , nFields
                , size
                , getFieldConf
                , formulas
                , types
                , isFormula
                , isVisible
                , fieldFormula
                , fieldType
                , fieldValues
                , nextPos
                -- **Updating
                , sortRows
                , sortRowsOn
                , setUnchanged
                , fieldIndex
                , setName
                , setNames
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
import Model.RowStore.RowStoreConf

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
                           , _visible :: Bool
                           } deriving Show

-- |The name of the `RowStore`
getName :: RowStore -> RowStoreName
getName = _nameRS

-- |Change the name of the `RowStore`
setName :: RowStoreName -> RowStore -> RowStore
setName n rst = rst { _nameRS = n }

-- |Determines the order in which updates must take place when a
-- `Field` changes
data UpdatePlan = UpdatePlan { expressions :: [Maybe Expression]
                             , influences :: IntMap [FieldPos]
                             , updateOrder :: [FieldPos]
                             , cycled :: [FieldPos]
                             } deriving Show

-- |Recovers a `RowStore` from the store
getRowStore :: RowStore -> Int -> RowStore
getRowStore = (!!) . _rowStores

-- |Finds the position of the `RowStore` with the given name
getRowStoreIndex :: RowStore -> Text -> Maybe Int
getRowStoreIndex rst name = findIndex ((== name) . _nameRS) $ _rowStores rst

-- |Get all the `RowStore` s
getRowStores :: RowStore -> [RowStore]
getRowStores = _rowStores

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

-- |The `DataSource` s of the `RowStore`
getDataSources :: RowStore -> [DataSource]
getDataSources = _dataSources

-- |Number of rows of the `RowStore`.
size :: RowStore -> Int
size = _size


-- |Transforms the `FieldInfo` into `FieldConf`.
toConf :: FieldInfo -> FieldConf
toConf inf = FieldConf { nameFC = _name inf
                       , typeFC = _type inf
                       , formulaFC = _formula inf
                       }


-- |Get the configuration of the fieds.
getFieldConf :: RowStore -> [FieldConf]
getFieldConf = map toConf . _fieldInfo


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

-- |True if the field is visible
isVisible :: FieldPos -> RowStore -> Bool
isVisible c = _visible . (!!! c) .  _fieldInfo

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
    v = convert (fieldType fpos store) [toField s]
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

-- |Returns the value of the visible properties of the fields
visibilities :: RowStore -> [Bool]
visibilities = map _visible . _fieldInfo

-- |The direction of a sort
data SortDirection = Ascending | Descending deriving Show

-- |Sort the rows of the store according to the field
sortRows :: FieldPos -> SortDirection -> RowStore -> RowStore
sortRows fp Ascending = sortRowsOn (!!! fp)
sortRows fp Descending = sortRowsOn (Down . (!!! fp))

-- |Sort the rows of the store on the provided function
sortRowsOn :: Ord a => (Row -> a) -> RowStore -> RowStore
sortRowsOn f rst = rst { _changed = True
                       , _rows = rows'
                       }
                 where rs = IM.elems $ _rows rst
                       sorted = sortOn f rs
                       rows' = IM.fromAscList $ zip [0..] sorted

-- |Marks the store as unchanged
setUnchanged :: RowStore -> RowStore
setUnchanged store = store { _changed = False }

