module DisplayInfo (
                   -- *Types
                   DisplayInfo(..)
) where

-- |The information to display
data DisplayInfo = DisplayInfo { fields :: [String]
                               , fieldNames :: [String]
                               , position :: Int
                               , modelSize :: Int
                               }
