module Presenter.File (
              -- *Types
              FileCommand(..)
              -- *Reexported
              , module Model.SourceInfo
) where

import Control.Auto(Auto, arrM)
import Control.Exception(try)

import GUI.Command
import HRowsException
import Model
import Model.ListatabFile
import Model.SourceInfo

data FileCommand = LoadFile
                 | LoadFileFromName FilePath (Maybe FilePath)
                 | WriteFile
                 | WriteFileFromName FilePath (Maybe FilePath)
                 | WriteBackup
                 | RemoveBackup
                 deriving Show

