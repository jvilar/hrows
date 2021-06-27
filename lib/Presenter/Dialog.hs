module Presenter.Dialog (
              -- *Types
              DialogCommand(..)
              -- *Reexported
              , ImportType(..)
) where

import Model
import Message
import Presenter.ImportType

data DialogCommand = LoadFileDialog
                   | SaveAsFileDialog
                   | CreateFieldsDialog
                   | DeleteFieldsDialog
                   | ImportFromDialog ImportType
                   | ChooseImportDialog ImportType RowStore
                   | RenameFieldsDialog
                   | ShowHideFieldsDialog
                   | SortRowsDialog
                   | MessageDialog Message
                   | ChangeFieldFormulaDialog FieldPos
                   | SearchFieldDialog FieldPos
                   | CopyOtherDialog FieldPos
                   | AddSourceDialog
                   | ShowSourcesDialog
                   | DeleteSourceDialog
                   | RenameSourcesDialog
                   | ShowAboutDialog
                     deriving Show
