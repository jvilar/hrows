module ListatabInfo (
                     ListatabInfo(..)
) where

data ListatabInfo = ListatabInfo { ltFileName :: FilePath
                                 , ltInputSeparator :: Char
                                 , ltOutputSeparator :: Char
                                 } deriving Show
