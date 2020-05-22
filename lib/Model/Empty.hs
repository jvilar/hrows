{-| A class for the types that have an empty value -}
module Model.Empty where

class Empty a where
  empty :: a

