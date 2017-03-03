module Presenter.File (
              -- *Types
              FileCommand(..)
) where

data FileCommand = LoadFile
                 | LoadFileFromName FilePath (Maybe FilePath)
                 | WriteFile
                 | WriteFileFromName FilePath (Maybe FilePath)
                 | ImportFieldsFromFileName FilePath Char
                 | WriteBackup
                 | RemoveBackup
                 deriving Show

