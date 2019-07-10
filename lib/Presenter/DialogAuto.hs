module Presenter.DialogAuto (
              -- *Functions
              dialogAuto
) where


import Control.Auto(arrM)

import GUI.Command
import Model
import Presenter.Auto
import Presenter.Input

dialogAuto :: PresenterAuto (DialogCommand, Model, RowPos) ()
dialogAuto = arrM $ \(input, model, pos) -> sendGUIM . ShowIteration $ case input of
                                             LoadFileDialog -> AskReadFile
                                             SaveAsFileDialog -> AskWriteFile
                                             CreateFieldsDialog -> AskCreateField
                                             DeleteFieldsDialog -> AskDeleteFields $ fnames model
                                             ImportFromDialog t -> AskImportFrom t
                                             ChooseImportDialog t m -> AskImportOptions t (fnames m) (fnames model) m
                                             ChangeNamesDialog -> AskRenameFields $ fnames model
                                             SortRowsDialog -> AskSortRows $ fnames model
                                             MessageDialog m -> DisplayMessage m
                                             ChangeFieldFormulaDialog f -> GetFieldFormula f (fnames model !! f) (formulas model !! f)
                                             SearchFieldDialog f -> SearchField f (toString $ row pos model !! f) (fieldValues f model)
                                             CopyOtherDialog f -> CopyOtherField f (toString $ row pos model !! f) (fieldValues f model)
