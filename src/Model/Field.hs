{-# LANGUAGE FlexibleInstances, OverloadedStrings, RankNTypes, TypeSynonymInstances #-}

module Model.Field ( Field
                   , FieldType(..)
                   -- *Classes
                   , ToField(..)
                   -- *Constants
                   , typeLabels
                   -- *Functions
                   , typeOf
                   , toString
                   , defaultValue
                   , mkError
                   , isError
                   , convert
                   ) where

import Data.Maybe(fromJust)
import Data.Text(Text)
import qualified Data.Text as T

-- |A field can store an Int, a Double or a String or it may be
-- empty. The special constructor AnError stores an erroneous string
-- for the type. It is useful for converting without loosing the
-- original value.
data Field = AInt Int
           | ADouble Double
           | AString String
           | AnError FieldType String
           | Empty
             deriving (Show, Eq)

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

typeLabel :: FieldType -> String
typeLabel t = T.unpack . fromJust $ lookup  t typeLabels

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

mkError :: String -> Field
mkError = AnError TypeEmpty

isError :: Field -> Bool
isError (AnError _ _) = True
isError _ = False

typeError :: String -> Field -> Field
typeError op f = AnError TypeEmpty $ "Error de tipos en " ++ op ++ ": " ++ typeLabel (typeOf f)

typeError2 :: String -> Field -> Field -> Field
typeError2 op f1 f2 = AnError TypeEmpty $ "Error de tipos en " ++ op ++ ": " ++ typeLabel (typeOf f1) ++ " y " ++ typeLabel (typeOf f2)

nmap :: String -> (forall a. Num a => a -> a) -> (Field -> Field)
nmap _ op (AInt n) = AInt (op n)
nmap _ op (ADouble d) = ADouble (op d)
nmap name _ f = typeError name f

instance Num Field where
    f@(AnError _ _) + _ = f
    _ + f@(AnError _ _) = f
    AInt n1 + AInt n2 = AInt (n1 + n2)
    AInt n + ADouble d = ADouble (fromIntegral n + d)

    ADouble d + AInt n = ADouble (d + fromIntegral n)
    ADouble d1 + ADouble d2 = ADouble (d1 + d2)

    AString s1 + AString s2 = AString (s1 ++ s2)

    f1 + f2 = typeError2 "suma" f1 f2

    f@(AnError _ _) * _ = f
    _ * f@(AnError _ _) = f
    AInt n1 * AInt n2 = AInt (n1 * n2)
    AInt n * ADouble d = ADouble (fromIntegral n * d)

    ADouble d * AInt n = ADouble (d * fromIntegral n)
    ADouble d1 * ADouble d2 = ADouble (d1 * d2)

    AString s * AInt n = AString (concat $ replicate n s)
    AInt n * AString s = AString (concat $ replicate n s)

    f1 * f2 = typeError2 "producto" f1 f2

    abs = nmap "valor absoluto" abs

    fromInteger = AInt . fromInteger

    negate = nmap "negación" negate

    signum = nmap "signum" signum

instance Real Field where
    toRational = undefined

instance Ord Field where
    compare (AInt n1) (AInt n2) = compare n1 n2
    compare (AInt n) (ADouble d) = compare (fromIntegral n) d
    compare (AInt _) _ = LT

    compare (ADouble d) (AInt n) = compare d (fromIntegral n)
    compare (ADouble d1) (ADouble d2) = compare d1 d2
    compare (ADouble _) _ = LT

    compare (AString s1) (AString s2) = compare s1 s2
    compare (AString _) (AInt _) = GT
    compare (AString _) (ADouble _) = GT
    compare (AString _) _ = LT

    compare (AnError _ s1) (AnError _ s2) = compare s1 s2
    compare (AnError _ _) (AInt _) = GT
    compare (AnError _ _) (ADouble _) = GT
    compare (AnError _ _) (AString _) = GT
    compare (AnError _ _) Empty = LT

    compare Empty Empty = EQ
    compare Empty _ = GT

instance Enum Field where
    toEnum = AInt

    fromEnum (AInt n) = n
    fromEnum f = error "fromEnum de un no entero"

instance Integral Field where
    quot (AInt n1) (AInt n2) = AInt (quot n1 n2)
    quot f1 f2 = typeError2 "división entera" f1 f2

    rem (AInt n1) (AInt n2) = AInt (rem n1 n2)
    rem f1 f2 = typeError2 "resto" f1 f2

    div (AInt n1) (AInt n2) = AInt (div n1 n2)
    div f1 f2 = typeError2 "división entera" f1 f2

    mod (AInt n1) (AInt n2) = AInt (mod n1 n2)
    mod f1 f2 = typeError2 "resto" f1 f2

    quotRem (AInt n1) (AInt n2) = (AInt q, AInt r)
        where (q, r) = quotRem n1 n2
    quotRem f1 f2 = ( typeError2 "división entera" f1 f2
                    , typeError2 "resto" f1 f2)

    divMod (AInt n1) (AInt n2) = (AInt d, AInt m)
        where (d, m) = divMod d m
    divMod f1 f2 = ( typeError2 "división entera" f1 f2
                    , typeError2 "resto" f1 f2)

    toInteger (AInt n) = toInteger n
    toInteger f = error $ "toInteger de un " ++ typeLabel (typeOf f)

instance Fractional Field where
    (AInt n1) / (AInt n2) = ADouble (fromIntegral n1 / fromIntegral n2)
    (AInt n) / (ADouble d) = ADouble (fromIntegral n / d)
    (ADouble d) / (AInt n) = ADouble (d / fromIntegral n)
    (ADouble d1) / (ADouble d2) = ADouble (d1/d2)
    f1 / f2 = typeError2 "división" f1 f2

    recip (AInt n) = ADouble (recip $ fromIntegral n)
    recip (ADouble d) = ADouble (recip d)
    recip f = typeError "recíproco" f

    fromRational = ADouble . fromRational

