module Presenter.Source (
    SourceCommand(..)
    ) where

import SourceInfo

data SourceCommand = SetSource SourceInfo
                   deriving Show