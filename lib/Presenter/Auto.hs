module Presenter.Auto (
    PresenterAuto
    , PresenterM
    , sendGUIA
    , sendGUIM
    , sendInputA
    , sendInputM
    ) where

import Control.Auto(Auto, arrM)
import Control.Monad.Writer.Strict(tell, WriterT)

import GUI.Command
import Presenter.Input

type PresenterM = WriterT [Either GUICommand Input] IO

type PresenterAuto = Auto PresenterM

sendGUIA :: PresenterAuto GUICommand ()
sendGUIA = arrM sendGUIM

sendGUIM :: GUICommand -> PresenterM ()
sendGUIM = tell . (:[]) . Left

sendInputA :: PresenterAuto Input ()
sendInputA = arrM sendInputM

sendInputM :: IsInput t => t -> PresenterM ()
sendInputM = tell . (:[]) . Right . toInput
