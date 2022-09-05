{-| A class for the types that have an empty value -}
module Model.Empty where

class Empty a where
  empty :: a

instance (Empty a, Empty b) => Empty (a, b) where
  empty = (empty, empty)

instance Empty (Maybe a) where
  empty = Nothing
