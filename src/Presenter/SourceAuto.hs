module Presenter.SourceAuto (
    sourceAuto
) where

import Control.Auto(Auto, accum_)

import Presenter.Input

import Presenter.Auto
import SourceInfo

sourceAuto :: SourceInfo -> PresenterAuto SourceCommand SourceInfo
sourceAuto = accum_ update

update :: SourceInfo -> SourceCommand -> SourceInfo
update _ (SetSource si) = si
