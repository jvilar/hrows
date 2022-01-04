{-# LANGUAGE OverloadedStrings #-}

module Model.RowStore.IO ( readRowStore
                         , readRowStoreStdin
                         , writeRowStore
                         ) where

import Control.Exception (throwIO, try)
import Data.Aeson(decode, FromJSON (parseJSON), Value)
import Data.Aeson.Encode.Pretty(encodePretty)
import Data.Aeson.Types(parseEither)
import qualified Data.ByteString.Lazy as BS

import HRowsException
import Model.RowStore.Base
import Model.RowStore.ListatabFile
import Model.RowStore.RowStoreConf
import Model.RowStore.Update
import Model.SourceInfo

-- |Reads a `RowStore` and its `RowStoreConf`(if any) using a `SourceInfo`
readRowStore :: SourceInfo -> IO (RowStore, Maybe RowStoreConf)
readRowStore si = do
       let PathAndConf fp mconfFp = siPathAndConf si
           ListatabFormat ltInfo = siFormat si
           name = siName si
       mconf <- readConf mconfFp
       let info = case mconf of
                      Nothing -> ltInfo
                      Just f -> case formatConf f of
                                    NoFormatInfo -> ltInfo
                                    ListatabFormat inf -> inf
       (h, ds) <- readListatab info $ Just fp
       rst <- case mconf of
           Nothing -> return $ case h of
                          Nothing -> fromRows name ds
                          Just ns -> fromRowsNames name ns ds
           Just cnf -> do
               let rs = fromRowsConf name cnf ds
               sources <- map fst <$> mapM readRowStore (sourceInfos cnf)
               return $ setUnchanged $ foldr addRowStore rs sources
       return (rst, mconf)

readRowStoreStdin :: ListatabInfo -> IO RowStore
readRowStoreStdin info = do
    (h, ds) <- readListatab info Nothing
    return $ case h of
                Nothing -> fromRows "stdin" ds
                Just ns -> fromRowsNames "stdin" ns ds

-- |Writes a `RowStore` using a `SourceInfo`
writeRowStore :: SourceInfo -> [SourceInfo] -> RowStore -> IO ()
writeRowStore si sInfos rs = do
      let PathAndConf fp mconfFp = siPathAndConf si
          ListatabFormat ltInfo = siFormat si
      writeListatab ltInfo fp (names rs) (rows rs)
      case mconfFp of
          Nothing -> return ()
          Just conf -> do
              r <- try (BS.writeFile conf . encodePretty . setSourceInfos sInfos $ getConf rs)
              case r of
                  Right () -> return ()
                  Left e -> exception e


parseConfFile :: BS.ByteString -> Either String RowStoreConf
parseConfFile bs = case (decode bs :: Maybe Value) of
                      Nothing -> Left "Erroneous JSON in config file"
                      Just v -> parseEither parseJSON v


readConf :: Maybe FilePath -> IO (Maybe RowStoreConf)
readConf Nothing = return Nothing
readConf (Just fp) = do
                 mf <- try $ do
                               l <- BS.readFile fp
                               if BS.length l > 0
                                   then return l
                                   else throwIO $ hRowsException $ "Empty config file: " ++ fp
                 case mf of
                     Right s -> case parseConfFile s of
                                    Left e -> throwIO $ hRowsException $ "Bad config file: " ++ fp ++ "\nException: " ++ show e
                                    Right c -> return $ Just c
                     Left e -> exception e

