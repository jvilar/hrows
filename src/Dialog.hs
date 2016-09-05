module Dialog (
              -- *Types
              DialogCommand(..)
) where

import AppState

data DialogCommand = LoadFileDialog
                   | DialogShown
                     deriving Show

instance StateUpdater DialogCommand where
    update LoadFileDialog s = undefined
    update DialogShown s = return s { pendingMessage = Nothing }
