{-# LANGUAGE DeriveGeneric, FlexibleInstances, OverloadedStrings, RankNTypes, TypeSynonymInstances #-}

module Model.Field ( Field
                   , FieldType(..)
                   , FieldPos
                   -- *Classes
                   , ToField(..)
                   -- *Constants
                   , typeLabels
                   -- *Functions
                   -- **General
                   , typeOf
                   , toString
                   , typeLabel
                   , typeOperator
                   , defaultValue
                   , mkError
                   , isError
                   , convert
                   -- **Operators
                   , andField
                   , orField
                   , compareField
                   , ternary
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
typeLabel = T.unpack . fromJust . flip lookup typeLabels

-- |The operators used for casting in formulas.
typeOperators :: [(FieldType, String)]
typeOperators = [ (TypeString, "str")
                , (TypeInt, "int")
                , (TypeInt0, "int0")
                , (TypeDouble, "float")
                , (TypeDouble0, "float0")
                ]

typeOperator :: FieldType -> String
typeOperator = fromJust . flip lookup typeOperators

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

doConvert (ADouble d str) TypeInt = AInt i $ show i
                                  where i = truncate d
doConvert (AString str) TypeInt = case reads str of
                                        [(n, "")] -> AInt n str
                                        _ -> AnError TypeInt str

doConvert (ADouble d _) TypeInt0 = AInt i $ show i
                                   where i = truncate d
doConvert (AString str) TypeInt0 = AInt ( case reads str of
                                                [(n, "")] -> n
                                                _ -> 0) str

doConvert (AInt n _) TypeDouble = ADouble f $ show f
                       where f = fromIntegral n
doConvert (AString str) TypeDouble = case reads str of
                                         [(d, "")] -> ADouble d str
                                         _ -> AnError TypeDouble str

doConvert (AInt n _) TypeDouble0 = ADouble f $ show f
                       where f = fromIntegral n
doConvert (AString str) TypeDouble0 = ADouble ( case reads str of
                                                    [(d, "")] -> d
                                                    _ -> 0 ) str
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

andField :: Field -> Field -> Field
andField e@(AnError _ _) _ = e
andField _ e@(AnError _ _) = e
andField (AInt n1 _) (AInt n2 _) | n1 > 0 && n2 > 0 = toField (1::Int)
                                 | otherwise = toField (0::Int)
andField f1 f2 = typeError2 "y lógico" f1 f2

orField :: Field -> Field -> Field
orField e@(AnError _ _) _ = e
orField _ e@(AnError _ _) = e
orField (AInt n1 _) (AInt n2 _) | n1 > 0 || n2 > 0 = toField (1::Int)
                                | otherwise = toField (0::Int)
orField f1 f2 = typeError2 "o lógico" f1 f2

compareField :: (Field -> Field -> Bool) -> Field -> Field -> Field
compareField _ e@(AnError _ _) _ = e
compareField _ _ e@(AnError _ _) = e
compareField cmp f1 f2 | not $ comparable (typeOf f1) (typeOf f2) = AnError TypeInt $ "Tipos no comparables: " ++ typeLabel (typeOf f1) ++ " y " ++ typeLabel (typeOf f2)
                       | cmp f1 f2 = toField (1::Int)
                       | otherwise = toField (0::Int)
    where comparable t1 t2 = numeric t1 && numeric t2 || t1 == t2
          numeric TypeInt = True
          numeric TypeInt0 = True
          numeric TypeDouble = True
          numeric TypeDouble0 = True
          numeric _ = False

ternary :: Field -> Field -> Field -> Field
ternary e@(AnError _ _) _ _ = e
ternary (AInt n1 _) e2 e3 | n1 > 0 = e2
                          | otherwise = e3
ternary e1 _ _ = typeError "operador ?" e1

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
