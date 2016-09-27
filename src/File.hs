module File (
              -- *Types
              FileCommand(..)
              -- *Reexported
              , module SourceInfo
) where

import Control.Auto(Auto, arrM)
import Control.Exception(try)

import GUI.Command
import HRowsException
import Iteration
import ListatabFile
import Model
import SourceInfo

data FileCommand = LoadFile
                 | LoadFileFromName FilePath
                 | WriteFile
                 | WriteFileFromName FilePath
                 deriving Show

