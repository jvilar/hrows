module Presenter.Dialog (
              -- *Types
              DialogCommand(..)
) where

import Message

data DialogCommand = LoadFileDialog
                   | SaveAsFileDialog
                   | CreateFieldsDialog
                   | DeleteFieldsDialog
                   | ChangeNamesDialog
                   | MessageDialog Message
                   | ChangeFieldFormulaDialog Int
                   | SearchFieldDialog Int
                     deriving Show
