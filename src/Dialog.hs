module Dialog (
              -- *Types
              DialogCommand(..)
) where

import AppState

data DialogCommand = LoadFileDialog
                   | DialogShown
                     deriving Show

instance StateUpdater DialogCommand where
    update LoadFileDialog s = return s { pendingIteration = AskReadFile }
    update DialogShown s = return s { pendingIteration = NoIteration }
