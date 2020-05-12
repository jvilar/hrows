module Presenter.File (
              -- *Types
              FileCommand(..)
) where

import Presenter.ImportType

data FileCommand = LoadFile
                 | LoadFileFromName FilePath (Maybe FilePath)
                 | WriteFile
                 | WriteFileFromName FilePath (Maybe FilePath)
                 | ImportFromFileName ImportType FilePath Char
                 | AddSourceFromFileName FilePath Char
                 | WriteBackup
                 | BackupOnExit
                 deriving Show

