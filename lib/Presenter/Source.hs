module Presenter.Source (
    SourceCommand(..)
    ) where

import Model.SourceInfo

data SourceCommand = SetMainSource SourceInfo
                   deriving Show
