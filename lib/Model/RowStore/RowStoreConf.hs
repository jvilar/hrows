{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.RowStore.RowStoreConf ( RowStoreConf(..)
                                   , mkRowStoreConf
                                   , fromListatabHeader
                                   , fromNamesTypes
                                   , fromNumberOfFields
                                   , FieldConf(..)
                                   , fromFieldConf
                                   , setSourceInfos
                                   , setFieldConf
                                   ) where

import Data.Aeson
import Data.Default(def)
import Data.Text(Text)
import Data.Vector(toList)
import GHC.Generics

import Model.Empty
import Model.Expression
import Model.SourceInfo

data RowStoreConf = RowStoreConf {
       fieldConf :: [FieldConf]
     , formatConf :: FormatInfo
     , sourceInfos :: [SourceInfo]
    } deriving (Generic, Show)

instance Empty RowStoreConf where
  empty = RowStoreConf [] def []

mkRowStoreConf :: [FieldConf] -> FormatInfo -> [SourceInfo] -> RowStoreConf
mkRowStoreConf = RowStoreConf

data FieldConf = FieldConf { nameFC :: Maybe Text
                           , typeFC :: FieldType
                           , formulaFC :: Maybe Formula
                           } deriving (Generic, Show)

-- |Creates a `RowStoreConf` when only the number of fields is known.
-- The types are set to `TypeString` and the names and formulae to
-- `Nothing`
fromNumberOfFields :: Int -> RowStoreConf
fromNumberOfFields nf = fromFieldConf
                      . replicate nf
                      $ FieldConf Nothing TypeString Nothing

-- |Creates a `RowStoreConf` from a `ListatabHeader`.
-- The types of Left fields are set to `TypeString` and all the formulae to
-- `Nothing`
fromListatabHeader :: ListatabHeader -> RowStoreConf
fromListatabHeader = fromFieldConf
                     . map (either (\n -> FieldConf (Just n) TypeString Nothing)
                                   (\(n, t) -> FieldConf (Just n) t Nothing))

-- |Creates a `RowStoreConf` using a list of names of Fields
-- and their types.
fromNamesTypes :: [Text] -> [FieldType] -> RowStoreConf
fromNamesTypes = (fromFieldConf .) . zipWith (\n t -> FieldConf (Just n) t Nothing)

fromFieldConf :: [FieldConf] -> RowStoreConf
fromFieldConf fc = setFieldConf fc empty

setSourceInfos :: [SourceInfo] -> RowStoreConf -> RowStoreConf
setSourceInfos si cnf = cnf { sourceInfos = si }

setFieldConf :: [FieldConf] -> RowStoreConf -> RowStoreConf
setFieldConf fc cnf = cnf { fieldConf = fc }

instance ToJSON RowStoreConf where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON RowStoreConf where
    parseJSON (Object v) = RowStoreConf
      <$> v .: "fieldConf"
      <*> v .: "formatConf"
      <*> v .: "sourceInfos"
    parseJSON (Array a) = fromFieldConf <$> mapM parseJSON (toList a)
    parseJSON _ = error "Mal formato del fichero de configuración"

instance ToJSON FieldConf where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON FieldConf
