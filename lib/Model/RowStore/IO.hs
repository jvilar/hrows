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

import HRowsException
import Model.RowStore.Base
import Model.RowStore.ListatabFile
import Model.RowStore.RowStoreConf
import Model.RowStore.Update
import Model.SourceInfo

-- |Reads a `RowStore` using a `SourceInfo`
readRowStore :: SourceInfo -> IO RowStore
readRowStore (SourceInfo Nothing _ _) = throwIO $ HRowsException "No puedo cargar un fichero vacío"
readRowStore (SourceInfo (Just fp) mconfFp (ListatabFormat ltInfo)) = do
  mconf <- readConf mconfFp
  let info = case formatConf <$> mconf of
                Nothing -> ltInfo
                Just NoFormatInfo -> ltInfo
                Just (ListatabFormat li) -> li
      fpt = T.pack fp
  (h, rs) <- readListatab info fp
  return $ case mconf of
             Nothing -> case h of
                            Nothing -> fromRows fpt rs
                            Just names -> fromRowsNames fpt names rs
             Just cnf -> fromRowsConf fpt cnf rs

-- /Writes a `RowStore` using a `SourceInfo`
writeRowStore :: SourceInfo -> [SourceInfo] -> RowStore -> IO ()
writeRowStore (SourceInfo Nothing _ _) _ _ = throwIO $ HRowsException "No puedo escribir si no sé el nombre del fichero"
writeRowStore (SourceInfo (Just fp) mconfFp (ListatabFormat ltInfo)) sInfos rs = do
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

