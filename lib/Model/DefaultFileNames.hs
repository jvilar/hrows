module Model.DefaultFileNames ( defaultConfFileName
                              , defaultBackupFileName
                              ) where

import System.FilePath((-<.>))

    -- |The default configuration file from a file name.
defaultConfFileName :: FilePath -> FilePath
defaultConfFileName = (-<.> "conf")

-- |The name of the backup file.
defaultBackupFileName :: FilePath -> FilePath
defaultBackupFileName = (++ ".hrows#")
