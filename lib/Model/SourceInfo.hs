{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Model.SourceInfo ( SourceInfo
                        , SourceName
                        , siName
                        , siFilePath
                        , siPathAndConf
                        , siFormat
                        , PathAndConf(..)
                        , mkPathAndConf
                        , FormatInfo(..)
                        , changeFormatInfo
                        , changePathAndConf
                        , mkSourceInfo
                        , renameSourceInfo
                        , module Model.RowStore.ListatabInfo
                        ) where

import Control.Monad(mzero)
import Data.Aeson (defaultOptions, genericToEncoding, FromJSON (parseJSON), ToJSON(..), Value(Object), (.:?), (.:))
import Data.Default(Default (def))
import Data.Maybe(fromMaybe)
import Data.Text(pack, Text)
import GHC.Generics (Generic)
import System.Directory (doesFileExist)
import System.FilePath(takeFileName)

import Model.RowStore.ListatabInfo
import Model.DefaultFileNames (defaultConfFileName)


-- |The name of a `SourceInfo`
type SourceName = Text


-- |The location of a file possibly including the corresponding configuration file
data PathAndConf = PathAndConf { path :: FilePath, confPath :: Maybe FilePath } deriving Show

-- |Build a `PathAndConf` from the path of the file and a `Maybe` of the configuration.
-- If the configuration is `Nothing` the file system is tested to see if it is appropriate
-- to use the default config file name (from `defaultConfFileName`). It will be used if the
-- file is new and the config does not exist or if both the file and the config file exist.
mkPathAndConf :: FilePath -> Maybe FilePath -> IO PathAndConf
mkPathAndConf fn Nothing = do
    let defFn = defaultConfFileName fn
    exFn <- doesFileExist fn
    exCnf <- doesFileExist defFn
    return . PathAndConf fn $ if exFn == exCnf
                              then Just defFn
                              else Nothing
mkPathAndConf fn cnf = return $ PathAndConf fn cnf

-- |The information about the source of the model. It contains the name,
-- the file path, and the options related to the format.
data SourceInfo = SourceInfo { siName :: SourceName
                             , siFilePath :: FilePath
                             , siConfFile :: Maybe FilePath
                             , siFormat :: FormatInfo
                             } deriving (Generic, Show)


instance ToJSON SourceInfo where
  toEncoding = genericToEncoding defaultOptions


instance FromJSON SourceInfo where
  parseJSON (Object v) = do
    n <- v .:? "siName"
    fp <- v .: "siFilePath"
    c <- v .: "siConfFile"
    f <- v .: "siFormat"
    return $ case n of
      Nothing -> SourceInfo (pack $ takeFileName fp) fp c f
      Just nm -> SourceInfo nm fp c f
  parseJSON _ = mzero


-- |This class defines the different formats available as input. For
-- the moment, only ListatabFormat is supported
data FormatInfo = NoFormatInfo
                | ListatabFormat ListatabInfo
                deriving (Generic, Show)


instance ToJSON FormatInfo where
    toEncoding = genericToEncoding defaultOptions


instance FromJSON FormatInfo


instance Default FormatInfo where
    def = ListatabFormat def


changePathAndConf :: PathAndConf -> SourceInfo -> SourceInfo
changePathAndConf pc si = si { siFilePath = path pc
                             , siConfFile = confPath pc
                             }


siPathAndConf :: SourceInfo -> PathAndConf
siPathAndConf (SourceInfo _ p cnf _) = PathAndConf p cnf


changeFormatInfo :: FormatInfo -> SourceInfo -> SourceInfo
changeFormatInfo f s = s { siFormat = f }


mkSourceInfo :: Maybe SourceName -> PathAndConf -> ListatabInfo -> SourceInfo
mkSourceInfo n PathAndConf {..} = let
    nme = fromMaybe (pack $ takeFileName path) n
  in SourceInfo nme path confPath . ListatabFormat


renameSourceInfo :: SourceName -> SourceInfo -> SourceInfo
renameSourceInfo n si = si { siName = n }
