module SourceAuto (
    sourceAuto
) where

import Control.Auto(Auto, accum_)

import Input

import PresenterAuto
import SourceInfo

sourceAuto :: SourceInfo -> PresenterAuto SourceCommand SourceInfo
sourceAuto = accum_ update

update :: SourceInfo -> SourceCommand -> SourceInfo
update _ (SetSource si) = si
