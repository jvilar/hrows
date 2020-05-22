{-# LANGUAGE DeriveGeneric #-}

module Model.SourceInfo ( SourceInfo(..)
                        , FormatInfo(..)
                        , FormatInfoClass(..)
                        , changeFileName
                        , changeConfFileName
                        , changeFormatInfo
                        , mkSourceInfo
                        , emptySourceInfo
                        , module Model.ListatabInfo
                        ) where

import Model.ListatabInfo
import GHC.Generics (Generic)
import Data.Aeson (defaultOptions, genericToEncoding, FromJSON, ToJSON(..))

-- |The information about the source of the model. It contains
-- the possible file path and the options related to te format.
data SourceInfo = SourceInfo { siFilePath :: Maybe FilePath
                             , siConfFile :: Maybe FilePath
                             , siFormat :: FormatInfo
                             } deriving (Generic, Show)

instance ToJSON SourceInfo where
  toEncoding = genericToEncoding defaultOptions
  
instance FromJSON SourceInfo

-- |The information about the format of the model.
data FormatInfo = NoFormatInfo
                | ListatabFormat ListatabInfo
                deriving (Generic, Show)
                
instance ToJSON FormatInfo where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON FormatInfo

changeFileName :: FilePath -> SourceInfo -> SourceInfo
changeFileName p s = s { siFilePath = Just p }

changeConfFileName :: Maybe FilePath -> SourceInfo -> SourceInfo
changeConfFileName mp s = s { siConfFile = mp }

changeFormatInfo :: FormatInfo -> SourceInfo -> SourceInfo
changeFormatInfo f s = s { siFormat = f }

mkSourceInfo :: FormatInfoClass i => Maybe FilePath -> Maybe FilePath -> i -> SourceInfo
mkSourceInfo p o = SourceInfo p o . toFormatInfo

emptySourceInfo :: SourceInfo
emptySourceInfo = mkSourceInfo Nothing Nothing ()

class FormatInfoClass t where
    toFormatInfo :: t -> FormatInfo

instance FormatInfoClass () where
    toFormatInfo () = NoFormatInfo

instance FormatInfoClass ListatabInfo where
    toFormatInfo = ListatabFormat

