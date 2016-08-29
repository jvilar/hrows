module SourceInfo ( SourceInfo(..)
                  , SourceInfoClass(..)
                  , module ListatabInfo
                  ) where


import ListatabInfo

-- |The information about the source of the model
data SourceInfo = EmptySource
                | ListatabSource ListatabInfo
                  deriving Show

class SourceInfoClass t where
    toSourceInfo :: t -> SourceInfo

instance SourceInfoClass () where
    toSourceInfo () = EmptySource

instance SourceInfoClass ListatabInfo where
    toSourceInfo = ListatabSource

