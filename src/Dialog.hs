module Dialog (
              -- *Types
              DialogCommand(..)
              -- *Functions
              , dialogAuto
) where


import Control.Arrow(arr)
import Control.Auto(Auto)

import GUICommand
import Iteration

data DialogCommand = LoadFileDialog
                   | SaveAsFileDialog
                     deriving Show

dialogAuto :: Auto IO DialogCommand [GUICommand]
dialogAuto = arr $ \input -> case input of
                               LoadFileDialog -> [ ShowIteration AskReadFile ]
                               SaveAsFileDialog -> [ ShowIteration AskWriteFile ]
