module Dialog (
              -- *Types
              DialogCommand(..)
) where

import AppState

data DialogCommand = LoadFileDialog
                   | SaveAsFileDialog
                   | DialogShown
                     deriving Show

instance StateUpdater DialogCommand where
    update LoadFileDialog s = return s { pendingIteration = AskReadFile }
    update SaveAsFileDialog s = return s { pendingIteration = AskWriteFile }
    update DialogShown s = return s { pendingIteration = NoIteration }
