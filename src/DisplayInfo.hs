module DisplayInfo (
                   -- *Types
                   DisplayInfo(..)
) where

import Message

-- |The information to display
data DisplayInfo = DisplayInfo { fields :: [String]
                               , fieldNames :: [String]
                               , position :: Int
                               , modelSize :: Int
                               , message :: Maybe Message
                               }
