module ListatabInfo ( ListatabInfo(..)
                    , HeaderType(..)
                    ) where

data ListatabInfo = ListatabInfo { ltFileName :: FilePath
                                 , ltInputSeparator :: Char
                                 , ltOutputSeparator :: Char
                                 , ltHeaderType :: HeaderType
                                 } deriving Show

data HeaderType = NoHeader
                | FirstLine
                | Comment
                deriving Show
