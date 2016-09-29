module Dialog (
              -- *Types
              DialogCommand(..)
) where

import Message

data DialogCommand = LoadFileDialog
                   | SaveAsFileDialog
                   | CreateFieldDialog
                   | DeleteFieldDialog
                   | MessageDialog Message
                     deriving Show
