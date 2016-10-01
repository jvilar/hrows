module Presenter.SourceAuto (
    sourceAuto
) where

import Control.Auto(Auto, accumM_)
import Data.Maybe(fromMaybe)

import Presenter.Input
import GUI.Command

import Presenter.Auto
import SourceInfo

sourceAuto :: SourceInfo -> PresenterAuto SourceCommand SourceInfo
sourceAuto = accumM_ update

update :: SourceInfo -> SourceCommand -> PresenterM SourceInfo
update _ (SetSource si) = do
    let name = "hrows: " ++ fromMaybe "No file" (siFilePath si)
    sendGUIM $ ChangeTitle name
    return si
