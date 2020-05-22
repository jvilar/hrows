{-# LANGUAGE DeriveGeneric #-}

module Model.ListatabInfo ( ListatabInfo(..)
                          , HeaderType(..)
                          ) where

import Data.Default(Default(..))
import GHC.Generics (Generic)
import Data.Aeson (defaultOptions, genericToEncoding, FromJSON, ToJSON(..))


-- |The information needed to read or write in listatab format
data ListatabInfo = ListatabInfo { ltInputSeparator :: Char
                                 , ltOutputSeparator :: Char
                                 , ltHeaderType :: HeaderType
                                 } deriving (Generic, Show)

instance ToJSON ListatabInfo where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON ListatabInfo

data HeaderType = NoHeader
                | FirstLine
                | Comment
                deriving (Generic, Show)

instance ToJSON HeaderType where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON HeaderType

instance Default ListatabInfo where
    def = ListatabInfo '\t' '\t' Comment


