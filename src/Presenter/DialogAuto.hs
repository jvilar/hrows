module Presenter.DialogAuto (
              -- *Functions
              dialogAuto
) where


import Control.Auto(arrM)

import GUI.Command
import Model
import Presenter.Input
import Presenter.Auto

dialogAuto :: PresenterAuto (DialogCommand, Model) ()
dialogAuto = arrM $ \(input, model) -> sendGUIM $ case input of
                                             LoadFileDialog -> ShowIteration AskReadFile
                                             SaveAsFileDialog -> ShowIteration AskWriteFile
                                             CreateFieldsDialog -> ShowIteration AskCreateField
                                             DeleteFieldsDialog -> ShowIteration (AskDeleteFields $ fnames model)
                                             MessageDialog m -> ShowIteration (DisplayMessage m)
                                             ChangeFieldFormulaDialog f -> ShowIteration $ GetFieldFormula f (fnames model !! f) (formulas model !! f)
