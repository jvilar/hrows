module Presenter.Source (
    SourceCommand(..)
    ) where

import Model.SourceInfo

data SourceCommand = SetSource SourceInfo
                   deriving Show
