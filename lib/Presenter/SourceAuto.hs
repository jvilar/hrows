module Presenter.SourceAuto (
    sourceAuto
) where

import Control.Auto(accumM_)
import Data.Maybe(fromMaybe)
import qualified Data.Text as T

import GUI.Command
import Model.SourceInfo
import Presenter.Input
import Presenter.Auto

sourceAuto :: SourceInfo -> PresenterAuto SourceCommand SourceInfo
sourceAuto = accumM_ update

update :: SourceInfo -> SourceCommand -> PresenterM SourceInfo
update _ (SetMainSource si) = do
    let name = "hrows: " ++ fromMaybe "No file" (path <$> siPathAndConf si)
    sendGUIM . ChangeTitle $ T.pack name
    return si
