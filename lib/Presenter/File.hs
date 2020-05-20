module Presenter.File (
              -- *Types
              FileCommand(..)
) where

import Presenter.ImportType
import Model.RowStore

data FileCommand = LoadFile
                 | LoadFileFromName FilePath (Maybe FilePath)
                 | WriteFile
                 | WriteFileFromName FilePath (Maybe FilePath)
                 | ImportFromFileName ImportType FilePath Char
                 | AddSourceFromFileName RowStoreName FilePath Char
                 | WriteBackup
                 | BackupOnExit
                 deriving Show

