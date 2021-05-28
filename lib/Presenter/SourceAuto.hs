module Presenter.SourceAuto (
    sourceAuto
) where

import Control.Auto(accumM_)
import qualified Data.Text as T

import GUI.Command
import Model.SourceInfo
import Presenter.Input
import Presenter.Auto

sourceAuto :: SourceInfo -> PresenterAuto SourceCommand SourceInfo
sourceAuto = accumM_ update

update :: SourceInfo -> SourceCommand -> PresenterM SourceInfo
update _ (SetMainSource si) = do
    sendGUIM . ChangeTitle . T.pack . path $ siPathAndConf si
    return si
