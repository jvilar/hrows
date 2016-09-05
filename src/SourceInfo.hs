module SourceInfo ( SourceInfo(..)
                  , SourceInfoClass(..)
                  , changeFileName
                  , module ListatabInfo
                  ) where


import ListatabInfo

-- |The information about the source of the model
data SourceInfo = EmptySource
                | ListatabSource ListatabInfo
                  deriving Show

class SourceInfoClass t where
    toSourceInfo :: t -> SourceInfo

changeFileName :: FilePath -> SourceInfo -> SourceInfo
changeFileName n EmptySource = EmptySource
changeFileName n (ListatabSource lt) = ListatabSource lt { ltFileName = n }

instance SourceInfoClass () where
    toSourceInfo () = EmptySource

instance SourceInfoClass ListatabInfo where
    toSourceInfo = ListatabSource

