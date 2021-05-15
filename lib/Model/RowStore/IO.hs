{-# LANGUAGE OverloadedStrings #-}

module Model.RowStore.IO (
    readRowStore,
    writeRowStore
) where

import Control.Exception (throwIO, try)
import Data.Aeson(decode)
import Data.Aeson.Encode.Pretty(encodePretty)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import System.FilePath(takeFileName)

import HRowsException
import Model.RowStore.Base
import Model.RowStore.ListatabFile
import Model.RowStore.RowStoreConf
import Model.RowStore.Update
import Model.SourceInfo

-- |Reads a `RowStore` and its `RowStoreConf`(if any) using a `SourceInfo`
readRowStore :: SourceInfo -> IO (RowStore, Maybe RowStoreConf)
readRowStore si = case siPathAndConf si of
    Nothing -> throwIO $ HRowsException "No puedo cargar un fichero vacío"
    Just (PathAndConf fp mconfFp) -> do
       let ListatabFormat ltInfo = siFormat si
       mconf <- readConf mconfFp
       let info = case formatConf <$> mconf of
                     Nothing -> ltInfo
                     Just NoFormatInfo -> ltInfo
                     Just (ListatabFormat li) -> li
           name = T.pack $ takeFileName fp
       (h, ds) <- readListatab info fp
       rst <- case mconf of
           Nothing -> case h of
                          Nothing -> return $ fromRows name ds
                          Just ns -> return $ fromRowsNames name ns ds
           Just cnf -> do
               let rs = fromRowsConf name cnf ds
               sources <- map fst <$> mapM readRowStore (sourceInfos cnf)
               return $ setUnchanged $ foldr addRowStore rs sources
       return (rst, mconf)


-- /Writes a `RowStore` using a `SourceInfo`
writeRowStore :: SourceInfo -> [SourceInfo] -> RowStore -> IO ()
writeRowStore si sInfos rs = case siPathAndConf si of
  Nothing -> throwIO $ HRowsException "No puedo escribir si no sé el nombre del fichero"
  Just (PathAndConf fp mconfFp) -> do
      let ListatabFormat ltInfo = siFormat si
      writeListatab ltInfo fp (names rs) (rows rs)
      case mconfFp of
          Nothing -> return ()
          Just conf -> do
              r <- try (BS.writeFile conf . encodePretty . setSourceInfos sInfos $ getConf rs)
              case r of
                  Right () -> return ()
                  Left e -> exception e


readConf :: Maybe FilePath -> IO (Maybe RowStoreConf)
readConf Nothing = return Nothing
readConf (Just fp) = do
                 mf <- try $ do
                               l <- BS.readFile fp
                               if BS.length l > 0
                                   then return l
                                   else throwIO $ hRowsException $ "Empty config file: " ++ fp
                 case mf of
                     Right s -> case decode s of
                                    Nothing -> throwIO $ hRowsException $ "Bad config file: " ++ fp
                                    Just c -> return $ Just c
                     Left e -> exception e

