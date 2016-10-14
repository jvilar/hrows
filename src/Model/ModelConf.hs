{-# LANGUAGE DeriveGeneric #-}

module Model.ModelConf ( ModelConf(..)
                       , FieldConf(..)
                       ) where

import Data.Aeson
import GHC.Generics

import Model.Expression
import Model.Field

newtype ModelConf = ModelConf [ FieldConf ] deriving (Generic, Show)

data FieldConf = FieldConf { nameFC :: Maybe String
                           , typeFC :: FieldType
                           , formulaFC :: Maybe Formula
                           } deriving (Generic, Show)

instance ToJSON ModelConf where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON ModelConf

instance ToJSON FieldConf where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON FieldConf
