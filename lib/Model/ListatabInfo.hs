module Model.ListatabInfo ( ListatabInfo(..)
                          , HeaderType(..)
                          ) where

import Data.Default(Default(..))

-- |The information needed to read or write in listatab format
data ListatabInfo = ListatabInfo { ltInputSeparator :: Char
                                 , ltOutputSeparator :: Char
                                 , ltHeaderType :: HeaderType
                                 } deriving Show

data HeaderType = NoHeader
                | FirstLine
                | Comment
                deriving Show

instance Default ListatabInfo where
    def = ListatabInfo '\t' '\t' Comment


