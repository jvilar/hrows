module Presenter.Dialog (
              -- *Types
              DialogCommand(..)
) where

import Message

data DialogCommand = LoadFileDialog
                   | SaveAsFileDialog
                   | CreateFieldsDialog
                   | DeleteFieldsDialog
                   | MessageDialog Message
                   | ChangeFieldFormulaDialog Int
                     deriving Show
