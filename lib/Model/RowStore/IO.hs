{-# LANGUAGE OverloadedStrings #-}

module Model.RowStore.IO (
    readRowStore,
    writeRowStore
) where

import Control.Exception (throwIO)

import HRowsException
import Model.RowStore.Base
import Model.RowStore.ListatabFile
import Model.SourceInfo

-- |Reads a `RowStore` using a `SourceInfo`
readRowStore :: SourceInfo -> IO RowStore
readRowStore (SourceInfo Nothing _ _) = throwIO $ HRowsException "No puedo cargar un fichero vacío"
readRowStore (SourceInfo (Just fp) conffp (ListatabFormat ltInfo)) = fromListatab ltInfo fp conffp

-- /Writes a `RowStore` using a `SourceInfo`
writeRowStore :: SourceInfo -> RowStore -> IO ()
writeRowStore (SourceInfo Nothing _ _) = const (throwIO $ HRowsException "No puedo escribir si no sé el nombre del fichero")
writeRowStore (SourceInfo (Just fp) conffp (ListatabFormat ltInfo)) = toListatab ltInfo fp conffp
