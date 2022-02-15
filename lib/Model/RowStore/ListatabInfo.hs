{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.RowStore.ListatabInfo ( ListatabInfo(..)
                                   , HeaderType(..)
                                   ) where

import Control.Applicative((<|>))
import Data.Default(Default(..))
import Data.Maybe(fromMaybe)
import GHC.Generics (Generic)
import Data.Aeson (defaultOptions, genericToEncoding, FromJSON(..), ToJSON(..), Value (Object), (.:?), (.:))


-- |The information needed to read or write in listatab format
data ListatabInfo = ListatabInfo { ltSeparator :: Char
                                 , ltHeaderType :: HeaderType
                                 } deriving (Generic, Show)

instance ToJSON ListatabInfo where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON ListatabInfo where
  parseJSON (Object v) = ListatabInfo
    <$> (do
            s <- (<|>) <$> (v .:? "ltSeparator")
                       <*> (v .:? "ltInputSeparator")
            return $ fromMaybe '\t' s
        )
    <*> v .: "ltHeaderType"
  parseJSON _ = error "Bad format of configuration file, problem reading a ListatabInfo"

data HeaderType = NoHeader
                | FirstLine
                | Comment
                deriving (Eq, Generic, Show)

instance ToJSON HeaderType where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON HeaderType

instance Default ListatabInfo where
    def = ListatabInfo '\t' Comment


