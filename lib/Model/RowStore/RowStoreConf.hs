{-# LANGUAGE DeriveGeneric #-}

module Model.RowStore.RowStoreConf ( RowStoreConf(..)
                                   , FieldConf(..)
                                   , fromFieldConf
                                   , setSourceInfos
                                   ) where

import Data.Aeson
import Data.Text(Text)
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
  empty = RowStoreConf [] NoFormatInfo []

data FieldConf = FieldConf { nameFC :: Maybe Text
                           , typeFC :: FieldType
                           , formulaFC :: Maybe Formula
                           } deriving (Generic, Show)

fromFieldConf :: [FieldConf] -> RowStoreConf
fromFieldConf fc = empty { fieldConf = fc }

setSourceInfos :: [SourceInfo] -> RowStoreConf -> RowStoreConf
setSourceInfos si cnf = cnf { sourceInfos = si }

instance ToJSON RowStoreConf where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON RowStoreConf

instance ToJSON FieldConf where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON FieldConf
