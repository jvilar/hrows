module ListatabInfo ( ListatabInfo(..)
                    , HeaderType(..)
                    ) where

data ListatabInfo = ListatabInfo { ltInputSeparator :: Char
                                 , ltOutputSeparator :: Char
                                 , ltHeaderType :: HeaderType
                                 } deriving Show

data HeaderType = NoHeader
                | FirstLine
                | Comment
                deriving Show
