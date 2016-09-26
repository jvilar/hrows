module DialogAuto (
              -- *Functions
              dialogAuto
) where


import Control.Auto(arrM)

import GUI.Command
import Input
import PresenterAuto

dialogAuto :: PresenterAuto DialogCommand ()
dialogAuto = arrM $ \input -> sendGUIM $ case input of
                                             LoadFileDialog -> ShowIteration AskReadFile
                                             SaveAsFileDialog -> ShowIteration AskWriteFile
