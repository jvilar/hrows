module Presenter.File (
              -- *Types
              FileCommand(..)
) where

import Presenter.ImportType
import Model.RowStore
import Model.SourceInfo (PathAndConf, SourceInfo)

data FileCommand = LoadFile
                 | LoadFileFromName PathAndConf
                 | WriteFile
                 | WriteFileFromName PathAndConf
                 | ImportFromFile ImportType SourceInfo
                 | AddSourceFromSourceInfo RowStoreName SourceInfo
                 | WriteBackup
                 | BackupOnExit
                 deriving Show

