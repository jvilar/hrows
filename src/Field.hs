{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Field ( Field
             , FieldType(..)
             -- *Classes
             , ToField(..)
             -- *Functions
             , typeOf
             , toString
             , defaultValue
) where

-- |A field can store an Int, a Double or a String or it may be empty.
data Field = AInt Int
           | ADouble Double
           | AString String
           | Empty
             deriving Show

class ToField t where
    toField :: t -> Field

instance ToField Int where
    toField = AInt

instance ToField Double where
    toField = ADouble

instance ToField String where
    toField = AString

instance ToField () where
    toField = const Empty

instance ToField f => ToField (Maybe f) where
    toField Nothing = Empty
    toField (Just v) = toField v

-- |The string associated to a `Field`.
toString :: Field -> String
toString (AInt n) = show n
toString (ADouble d) = show d
toString (AString s) = s
toString Empty = "---"

data FieldType = TypeInt
               | TypeDouble
               | TypeString
               | TypeEmpty
               deriving Show

typeOf :: Field -> FieldType
typeOf (AInt _) = TypeInt
typeOf (ADouble _) = TypeDouble
typeOf (AString _) = TypeString
typeOf Empty = TypeEmpty

defaultValue :: FieldType -> Field
defaultValue TypeInt = AInt 0
defaultValue TypeDouble = ADouble 0
defaultValue TypeString = AString ""
defaultValue TypeEmpty = Empty
