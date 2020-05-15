{-# LANGUAGE DeriveGeneric #-}

module Model.RowStoreConf ( RowStoreConf(..)
                          , FieldConf(..)
                          ) where

import Data.Aeson
import Data.Text(Text)
import GHC.Generics

import Model.Expression

newtype RowStoreConf = RowStoreConf [ FieldConf ] deriving (Generic, Show)

data FieldConf = FieldConf { nameFC :: Maybe Text
                           , typeFC :: FieldType
                           , formulaFC :: Maybe Formula
                           } deriving (Generic, Show)

instance ToJSON RowStoreConf where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON RowStoreConf

instance ToJSON FieldConf where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON FieldConf
