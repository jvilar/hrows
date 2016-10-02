{-# LANGUAGE FlexibleInstances, OverloadedStrings, TypeSynonymInstances #-}

module Field ( Field
             , FieldType(..)
             -- *Classes
             , ToField(..)
             -- *Constants
             , typeLabels
             -- *Functions
             , typeOf
             , toString
             , defaultValue
             , isError
             , convert
) where

import Data.Text(Text)

-- |A field can store an Int, a Double or a String or it may be
-- empty. The special constructor AnError stores an erroneous string
-- for the type. It is useful for converting without loosing the
-- original value.
data Field = AInt Int
           | ADouble Double
           | AString String
           | AnError FieldType String
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

-- |The types that can be seen by the user and their labels
typeLabels :: [(FieldType, Text)]
typeLabels = [ (TypeString, "Cadena")
             , (TypeInt, "Entero")
             , (TypeDouble, "Flotante")
             ]

-- |The string associated to a `Field`.
toString :: Field -> String
toString (AInt n) = show n
toString (ADouble d) = show d
toString (AString s) = s
toString (AnError _ s) = s
toString Empty = "-----"

data FieldType = TypeInt
               | TypeDouble
               | TypeString
               | TypeEmpty
               deriving (Show, Eq)

typeOf :: Field -> FieldType
typeOf (AInt _) = TypeInt
typeOf (ADouble _) = TypeDouble
typeOf (AString _) = TypeString
typeOf (AnError t _) = t
typeOf Empty = TypeEmpty

defaultValue :: FieldType -> Field
defaultValue TypeInt = AInt 0
defaultValue TypeDouble = ADouble 0
defaultValue TypeString = AString ""
defaultValue TypeEmpty = Empty

convert :: Field -> FieldType -> Field
convert f t | typeOf f == t = f
            | otherwise = doConvert f t

doConvert :: Field -> FieldType -> Field
doConvert f TypeInt = case reads str of
                        [(n, "")] -> AInt n
                        _ -> AnError TypeInt str
                    where str = toString f
doConvert f TypeDouble = case reads str of
                           [(d, "")] -> ADouble d
                           _ -> AnError TypeDouble str
                       where str = toString f
doConvert f TypeString = AString $ toString f
doConvert f TypeEmpty = AnError TypeEmpty $ toString f

isError :: Field -> Bool
isError (AnError _ _) = True
isError _ = False
