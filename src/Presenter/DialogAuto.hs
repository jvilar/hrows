module Presenter.DialogAuto (
              -- *Functions
              dialogAuto
) where


import Control.Auto(arrM)

import GUI.Command
import Presenter.Input
import Presenter.Auto

dialogAuto :: PresenterAuto DialogCommand ()
dialogAuto = arrM $ \input -> sendGUIM $ case input of
                                             LoadFileDialog -> ShowIteration AskReadFile
                                             SaveAsFileDialog -> ShowIteration AskWriteFile
                                             CreateFieldDialog -> ShowIteration AskCreateField
                                             DeleteFieldDialog -> ShowIteration AskDeleteField
                                             MessageDialog m -> ShowIteration (DisplayMessage m)
