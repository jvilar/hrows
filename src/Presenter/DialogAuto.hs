module Presenter.DialogAuto (
              -- *Functions
              dialogAuto
) where


import Control.Auto(arrM)

import GUI.Command
import Model
import Presenter.Input
import Presenter.Auto

dialogAuto :: PresenterAuto (DialogCommand, Model, RowPos) ()
dialogAuto = arrM $ \(input, model, pos) -> sendGUIM $ case input of
                                             LoadFileDialog -> ShowIteration AskReadFile
                                             SaveAsFileDialog -> ShowIteration AskWriteFile
                                             CreateFieldsDialog -> ShowIteration AskCreateField
                                             DeleteFieldsDialog -> ShowIteration (AskDeleteFields $ fnames model)
                                             ImportFieldsFromDialog -> ShowIteration AskImportFieldsFrom
                                             ChooseImportFieldsDialog m -> ShowIteration (AskImportFieldsOptions (fnames m) (fnames model) m)
                                             ChangeNamesDialog -> ShowIteration (AskRenameFields $ fnames model)
                                             SortRowsDialog -> ShowIteration (AskSortRows $ fnames model)
                                             MessageDialog m -> ShowIteration (DisplayMessage m)
                                             ChangeFieldFormulaDialog f -> ShowIteration $ GetFieldFormula f (fnames model !! f) (formulas model !! f)
                                             SearchFieldDialog f -> ShowIteration $ SearchField f (toString $ row pos model !! f) (fieldValues f model)
                                             CopyOtherDialog f -> ShowIteration $ CopyOtherField f (toString $ row pos model !! f) (fieldValues f model)
