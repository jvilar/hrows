module Dialog (
              -- *Types
              DialogCommand(..)
) where

data DialogCommand = LoadFileDialog
                   | SaveAsFileDialog
                     deriving Show
