{-# LANGUAGE DeriveGeneric, FlexibleInstances, OverloadedStrings, RankNTypes, TypeSynonymInstances #-}

module Model.Field ( Field
                   , FieldType(..)
                   , FieldPos
                   -- *Classes
                   , ToField(..)
                   -- *Constants
                   , typeLabels
                   -- *Functions
                   , typeOf
                   , toString
                   , typeLabel
                   , defaultValue
                   , mkError
                   , isError
                   , convert
                   ) where

import Data.Aeson
import Data.Maybe(fromJust)
import Data.Text(Text)
import qualified Data.Text as T
import GHC.Generics

-- |A field can store an Int, a Double or a String or it may be
-- empty. The special constructor AnError stores an erroneous string
-- for the type. It is useful for converting without loosing the
-- original value.
data Field = AInt Int String
           | ADouble Double String
           | AString String
           | AnError FieldType String
           | Empty
             deriving (Show, Eq)

type FieldPos = Int

class ToField t where
    toField :: t -> Field

instance ToField Int where
    toField n = AInt n (show n)

instance ToField Double where
    toField d = ADouble d (show d)

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
             , (TypeInt0, "Entero0")
             , (TypeDouble, "Flotante")
             , (TypeDouble0, "Flotante0")
             ]

typeLabel :: FieldType -> String
typeLabel t = T.unpack . fromJust $ lookup  t typeLabels

-- |The string associated to a `Field`.
toString :: Field -> String
toString (AInt _ s) = s
toString (ADouble _ s) = s
toString (AString s) = s
toString (AnError _ s) = s
toString Empty = "-----"

data FieldType = TypeInt
               | TypeInt0
               | TypeDouble
               | TypeDouble0
               | TypeString
               | TypeEmpty
               deriving (Show, Eq, Generic)

instance ToJSON FieldType where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON FieldType

typeOf :: Field -> FieldType
typeOf (AInt _ _) = TypeInt
typeOf (ADouble _ _) = TypeDouble
typeOf (AString _) = TypeString
typeOf (AnError t _) = t
typeOf Empty = TypeEmpty

defaultValue :: FieldType -> Field
defaultValue TypeInt = AInt 0 "0"
defaultValue TypeInt0 = AInt 0 ""
defaultValue TypeDouble = ADouble 0 "0.0"
defaultValue TypeDouble0 = ADouble 0 ""
defaultValue TypeString = AString ""
defaultValue TypeEmpty = Empty

convert :: FieldType -> Field -> Field
convert t f | typeOf f == t = f
            | otherwise = doConvert f t

doConvert :: Field -> FieldType -> Field
doConvert (AnError _ m) t = AnError t m
doConvert f TypeInt = case reads str of
                        [(n, "")] -> AInt n str
                        _ -> AnError TypeInt str
                    where str = toString f
doConvert f TypeInt0 = AInt ( case reads str of
                                 [(n, "")] -> n
                                 _ -> 0) str
                    where str = toString f
doConvert f TypeDouble = case reads str of
                           [(d, "")] -> ADouble d str
                           _ -> AnError TypeDouble str
                       where str = toString f
doConvert f TypeDouble0 = ADouble ( case reads str of
                                      [(d, "")] -> d
                                      _ -> 0 ) str
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
nmap _ op (AInt n _) = toField $ op n
nmap _ op (ADouble d _) = toField $ op d
nmap name _ f = typeError name f

instance Num Field where
    f@(AnError _ _) + _ = f
    _ + f@(AnError _ _) = f
    AInt n1 _ + AInt n2 _ = toField $ n1 + n2
    AInt n _ + ADouble d _ = toField $ fromIntegral n + d

    ADouble d _ + AInt n _ = toField $ d + fromIntegral n
    ADouble d1 _ + ADouble d2 _ = toField $ d1 + d2

    AString s1 + AString s2 = AString (s1 ++ s2)

    f1 + f2 = typeError2 "suma" f1 f2

    f@(AnError _ _) * _ = f
    _ * f@(AnError _ _) = f
    AInt n1 _ * AInt n2 _ = toField $ n1 * n2
    AInt n _ * ADouble d _ = toField $ fromIntegral n * d

    ADouble d _ * AInt n _ = toField $ d * fromIntegral n
    ADouble d1 _ * ADouble d2 _ = toField $ d1 * d2

    AString s * AInt n _ = AString (concat $ replicate n s)
    AInt n _ * AString s = AString (concat $ replicate n s)

    f1 * f2 = typeError2 "producto" f1 f2

    abs = nmap "valor absoluto" abs

    fromInteger v = AInt (fromInteger v) (show v)

    negate = nmap "negación" negate

    signum = nmap "signum" signum

instance Real Field where
    toRational = undefined

instance Ord Field where
    compare (AInt n1 _) (AInt n2 _) = compare n1 n2
    compare (AInt n _) (ADouble d _) = compare (fromIntegral n) d
    compare (AInt _ _) _ = LT

    compare (ADouble d _) (AInt n _) = compare d (fromIntegral n)
    compare (ADouble d1 _) (ADouble d2 _) = compare d1 d2
    compare (ADouble _ _) _ = LT

    compare (AString s1) (AString s2) = compare s1 s2
    compare (AString _) (AInt _ _) = GT
    compare (AString _) (ADouble _ _) = GT
    compare (AString _) _ = LT

    compare (AnError _ s1) (AnError _ s2) = compare s1 s2
    compare (AnError _ _) (AInt _ _) = GT
    compare (AnError _ _) (ADouble _ _) = GT
    compare (AnError _ _) (AString _) = GT
    compare (AnError _ _) Empty = LT

    compare Empty Empty = EQ
    compare Empty _ = GT

instance Enum Field where
    toEnum v = AInt v (show v)

    fromEnum (AInt n _) = n
    fromEnum _ = error "fromEnum de un no entero"

instance Integral Field where
    quot (AInt n1 _) (AInt n2 _) = toField $ quot n1 n2
    quot f1 f2 = typeError2 "división entera" f1 f2

    rem (AInt n1 _) (AInt n2 _) = toField $ rem n1 n2
    rem f1 f2 = typeError2 "resto" f1 f2

    div (AInt n1 _) (AInt n2 _) = toField $ div n1 n2
    div f1 f2 = typeError2 "división entera" f1 f2

    mod (AInt n1 _) (AInt n2 _) = toField $ mod n1 n2
    mod f1 f2 = typeError2 "resto" f1 f2

    quotRem (AInt n1 _) (AInt n2 _) = (toField q, toField r)
        where (q, r) = quotRem n1 n2
    quotRem f1 f2 = ( typeError2 "división entera" f1 f2
                    , typeError2 "resto" f1 f2)

    divMod (AInt n1 _) (AInt n2 _) = (toField d, toField m)
        where (d, m) = divMod n1 n2
    divMod f1 f2 = ( typeError2 "división entera" f1 f2
                    , typeError2 "resto" f1 f2)

    toInteger (AInt n _) = toInteger n
    toInteger f = error $ "toInteger de un " ++ typeLabel (typeOf f)

instance Fractional Field where
    (AInt n1 _) / (AInt n2 _) = toField ((fromIntegral n1 / fromIntegral n2)::Double)
    (AInt n _) / (ADouble d _) = toField $ fromIntegral n / d
    (ADouble d _) / (AInt n _) = toField $ d / fromIntegral n
    (ADouble d1 _) / (ADouble d2 _) = toField $ d1/d2
    f1 / f2 = typeError2 "división" f1 f2

    recip (AInt n _) = toField ((recip $ fromIntegral n) :: Double)
    recip (ADouble d _) = toField $ recip d
    recip f = typeError "recíproco" f

    fromRational v = toField (fromRational v :: Double)
