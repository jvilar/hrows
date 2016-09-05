module DisplayInfo (
                   -- *Types
                   DisplayInfo(..)
) where

import Iteration

-- |The information to display
data DisplayInfo = DisplayInfo { fields :: [String]
                               , fieldNames :: [String]
                               , position :: Int
                               , modelSize :: Int
                               , iteration :: Iteration
                               }
