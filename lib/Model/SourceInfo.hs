{-# LANGUAGE DeriveGeneric #-}

module Model.SourceInfo ( SourceInfo
                        , siPathAndConf
                        , siFormat
                        , PathAndConf(..)
                        , FormatInfo(..)
                        , FormatInfoClass(..)
                        , changeFormatInfo
                        , changePathAndConf
                        , mkSourceInfo
                        , module Model.RowStore.ListatabInfo
                        ) where

import Model.Empty
import Model.RowStore.ListatabInfo
import GHC.Generics (Generic)
import Data.Aeson (defaultOptions, genericToEncoding, FromJSON, ToJSON(..))


-- |The location of a file possibly including the corresponding configuration file
data PathAndConf = PathAndConf { path :: FilePath, confPath :: Maybe FilePath } deriving Show


-- |The information about the source of the model. It contains
-- the possible file path and the options related to the format.
data SourceInfo = SourceInfo { siFilePath :: Maybe FilePath
                             , siConfFile :: Maybe FilePath
                             , siFormat :: FormatInfo
                             } deriving (Generic, Show)

instance Empty SourceInfo where
  empty = mkSourceInfo Nothing ()

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

changePathAndConf :: PathAndConf -> SourceInfo -> SourceInfo
changePathAndConf pc si = si { siFilePath = Just $ path pc
                             , siConfFile = confPath pc
                             }

siPathAndConf :: SourceInfo -> Maybe PathAndConf
siPathAndConf (SourceInfo Nothing _ _) = Nothing
siPathAndConf (SourceInfo (Just p) cnf _) = Just $ PathAndConf p cnf

changeFormatInfo :: FormatInfo -> SourceInfo -> SourceInfo
changeFormatInfo f s = s { siFormat = f }

mkSourceInfo :: FormatInfoClass i => Maybe PathAndConf -> i -> SourceInfo
mkSourceInfo pc = SourceInfo (path <$> pc) (pc >>= confPath) . toFormatInfo

class FormatInfoClass t where
    toFormatInfo :: t -> FormatInfo

instance FormatInfoClass () where
    toFormatInfo () = NoFormatInfo

instance FormatInfoClass ListatabInfo where
    toFormatInfo = ListatabFormat

