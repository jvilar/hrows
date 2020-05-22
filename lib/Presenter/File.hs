module Presenter.File (
              -- *Types
              FileCommand(..)
) where

import Presenter.ImportType
import Model.RowStore
import Model.SourceInfo (SourceInfo)

data FileCommand = LoadFile
                 | LoadFileFromName FilePath (Maybe FilePath)
                 | WriteFile
                 | WriteFileFromName FilePath (Maybe FilePath)
                 | ImportFromFileName ImportType FilePath Char
                 | AddSourceFromSourceInfo RowStoreName SourceInfo
                 | WriteBackup
                 | BackupOnExit
                 deriving Show

