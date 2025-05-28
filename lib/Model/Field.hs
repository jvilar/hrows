{-# LANGUAGE DeriveGeneric, FlexibleInstances, OverloadedStrings, RankNTypes #-}

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
                   , convertKeepText
                   , toInt
                   , toDouble
                   -- **Operators
                   , andField
                   , orField
                   , notField
                   , compareField
                   , maxField
                   , minField
                   , intsInParentheses
                   , ternary
                   -- *Other
                   , (!!!)
                   ) where

import Data.Aeson
import Data.Maybe(fromJust, fromMaybe)
import Data.Text(Text)
import qualified Data.Text as T
import Data.Text.Read(decimal, signed, double)
import GHC.Int(Int32)
import Numeric (showFFloat)
import TextShow(TextShow(showt))

import GHC.Generics

-- |A field can store an Int, a Double or a String or it may be
-- empty. The special constructor AnError stores an erroneous string
-- for the type. It is useful for converting without loosing the
-- original value.
data Field = AnInt Int Text
           | ADouble Double Text
           | AString Text
           | AnError FieldType Text
           | Empty
             deriving (Show, Eq)

-- |The position of the field, it is an Int32 for compatibility with gi-gtk
type FieldPos = Int32

-- |Auxiliary operator for accessing a list with a FieldPos
(!!!) :: [a] -> FieldPos -> a
(!!!) l =  (!!) l . fromIntegral


class ToField t where
    toField :: t -> Field

instance ToField Int where
    toField n = AnInt n $ showt n

instance ToField Double where
    toField d = ADouble d $ showt d

instance ToField Text where
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

typeLabel :: FieldType -> Text
typeLabel = fromMaybe "Vacío" . (`lookup` typeLabels)

-- |The operators used for casting in formulas.
typeOperators :: [(FieldType, Text)]
typeOperators = [ (TypeString, "str")
                , (TypeInt, "int")
                , (TypeInt0, "int0")
                , (TypeDouble, "float")
                , (TypeDouble0, "float0")
                ]

typeOperator :: FieldType -> Text
typeOperator = fromJust . flip lookup typeOperators

-- |The string associated to a `Field`.
toString :: Field -> Text
toString (AnInt _ s) = s
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
               deriving (Show, Eq, Generic, Read, Enum, Bounded)

instance ToJSON FieldType where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON FieldType

typeOf :: Field -> FieldType
typeOf (AnInt _ _) = TypeInt
typeOf (ADouble _ _) = TypeDouble
typeOf (AString _) = TypeString
typeOf (AnError t _) = t
typeOf Empty = TypeEmpty

defaultValue :: FieldType -> Field
defaultValue TypeInt = AnInt 0 "0"
defaultValue TypeInt0 = AnInt 0 ""
defaultValue TypeDouble = ADouble 0 "0.0"
defaultValue TypeDouble0 = ADouble 0 ""
defaultValue TypeString = AString ""
defaultValue TypeEmpty = Empty

baseType :: FieldType -> FieldType
baseType = typeOf . defaultValue

-- |Convert a field to a given type, return `AnError` with
-- a message if there is an error in the conversion
convert :: FieldType -> [Field] -> Field
convert t [f] | typeOf f == baseType t = f
              | otherwise = doConvert f t
convert TypeString [f, d] | typeOf f == TypeDouble && typeOf d == TypeInt = convertWithDecimals f d
convert t fs = AnError t $ T.concat ["Error en conversión a ", T.pack $ show t, ": parámetros: ", T.intercalate ", " $ map toString fs]

-- |Convert to a double to a string with the given number of decimals
convertWithDecimals :: Field -> Field -> Field
convertWithDecimals (ADouble d _) (AnInt n _) = AString . T.pack $ showFFloat (Just n) d ""

-- |Convert a field to the given type but keeping the text
convertKeepText :: FieldType -> Field -> Field
convertKeepText t f = case convert t [f] of
                        AnError _ _ -> AnError t (toString f)
                        f' -> f'

doConvert :: Field -> FieldType -> Field
doConvert (AnError _ m) t = AnError t m

doConvert (ADouble d _) TypeInt = AnInt i $ showt i
                                  where i = truncate d
doConvert (AString str) TypeInt = case signed decimal str of
                                        Right (n, "") -> AnInt n str
                                        _ -> AnError TypeInt (str `T.append` " no es un entero")

doConvert (ADouble d _) TypeInt0 = AnInt i $ showt i
                                   where i = truncate d
doConvert (AString str) TypeInt0 = AnInt (case signed decimal str of
                                                Right (n, "") -> n
                                                _ -> 0) str

doConvert (AnInt n _) TypeDouble = ADouble f $ showt f
                       where f = fromIntegral n
doConvert (AString str) TypeDouble = case signed double str of
                                         Right (d, "") -> ADouble d str
                                         _ -> AnError TypeDouble (str `T.append` " no es un flotante")

doConvert (AnInt n _) TypeDouble0 = ADouble f $ showt f
                       where f = fromIntegral n
doConvert (AString str) TypeDouble0 = ADouble (case signed double str of
                                                    Right (d, "") -> d
                                                    _ -> 0 ) str
doConvert Empty t = AnError t "Conversión de valor vacío"

doConvert f TypeString = AString $ toString f
doConvert f t = AnError t $ toString f

mkError :: Text -> Field
mkError = AnError TypeEmpty

isError :: Field -> Bool
isError AnError {} = True
isError _ = False

typeError :: Text -> Field -> Field
typeError _ e@AnError {} = e
typeError op f = AnError TypeEmpty $ T.concat ["Error de tipos en ", op, ": ", typeLabel $ typeOf f]

typeError2 :: Text -> Field -> Field -> Field
typeError2 _ e@AnError{} _ = e
typeError2 _ _ e@AnError{} = e
typeError2 op f1 f2 = AnError TypeEmpty $ T.concat ["Error de tipos en ", op, ": ", typeLabel $ typeOf f1, " y ", typeLabel $ typeOf f2]

nmap :: Text -> (forall a. Num a => a -> a) -> (Field -> Field)
nmap _ op (AnInt n _) = toField $ op n
nmap _ op (ADouble d _) = toField $ op d
nmap name _ f = typeError name f

andField :: Field -> Field -> Field
andField (AnInt n1 _) (AnInt n2 _) | n1 > 0 && n2 > 0 = toField (1::Int)
                                 | otherwise = toField (0::Int)
andField f1 f2 = typeError2 "y lógico" f1 f2

orField :: Field -> Field -> Field
orField (AnInt n1 _) (AnInt n2 _) | n1 > 0 || n2 > 0 = toField (1::Int)
                                | otherwise = toField (0::Int)
orField f1 f2 = typeError2 "o lógico" f1 f2

notField :: Field -> Field
notField (AnInt n _) | n > 0 = toField (0::Int)
                     | otherwise = toField (1::Int)
notField f = typeError "no lógico" f

maxField :: [Field] -> Field
maxField [] = AnError TypeInt "max de lista vacía"
maxField fs = extreme (>=) fs

minField :: [Field] -> Field
minField [] = AnError TypeInt "min de lista vacía"
minField fs = extreme (<=) fs

extreme :: (Field -> Field -> Bool) -> [Field] -> Field
extreme cmp fs = go fs
   where go [] = AnError TypeInt "extremo de lista vacía"
         go [f] = f
         go (f1:f2:fs) = case compareField cmp f1 f2 of
                            err@(AnError _ _) -> err
                            AnInt 1 _ -> go $ f1:fs
                            _ -> go $ f2:fs

intsInParentheses :: [Field] -> Field
intsInParentheses [AString s] = toField $ go s 0
    where go txt n | T.null txt = n :: Int
                   | otherwise = let
                        rest = T.dropWhile (/= '(') txt
                      in case T.uncons rest of
                           Nothing -> n
                           Just (_, rest') -> case signed decimal rest' of
                                                Right (m, rest'') -> go rest'' (n + m)
                                                _ -> go rest' n
intsInParentheses f = AnError TypeInt $ "intsInParentheses, parámetros erróneos: " <> T.intercalate ", " (map toString f)

compareField :: (Field -> Field -> Bool) -> Field -> Field -> Field
compareField _ e@(AnError _ _) _ = e
compareField _ _ e@(AnError _ _) = e
compareField cmp f1 f2 | not $ comparable (typeOf f1) (typeOf f2) = AnError TypeInt $ T.concat ["Tipos no comparables: ", typeLabel $ typeOf f1, " y ", typeLabel $ typeOf f2]
                       | cmp f1 f2 = toField (1::Int)
                       | otherwise = toField (0::Int)
    where comparable t1 t2 = numeric t1 && numeric t2 || t1 == t2
          numeric TypeInt = True
          numeric TypeInt0 = True
          numeric TypeDouble = True
          numeric TypeDouble0 = True
          numeric _ = False

ternary :: Field -> Field -> Field -> Field
ternary (AnInt n1 _) e2 e3 | n1 > 0 = e2
                           | otherwise = e3
ternary e1 _ _ = typeError "operador ?" e1

instance Num Field where
    AnInt n1 _ + AnInt n2 _ = toField $ n1 + n2
    AnInt n _ + ADouble d _ = toField $ fromIntegral n + d

    ADouble d _ + AnInt n _ = toField $ d + fromIntegral n
    ADouble d1 _ + ADouble d2 _ = toField $ d1 + d2

    AString s1 + AString s2 = AString $ T.append s1 s2

    f1 + f2 = typeError2 "suma" f1 f2

    AnInt n1 _ * AnInt n2 _ = toField $ n1 * n2
    AnInt n _ * ADouble d _ = toField $ fromIntegral n * d

    ADouble d _ * AnInt n _ = toField $ d * fromIntegral n
    ADouble d1 _ * ADouble d2 _ = toField $ d1 * d2

    AString s * AnInt n _ = AString $ T.replicate n s
    AnInt n _ * AString s = AString $ T.replicate n s

    f1 * f2 = typeError2 "producto" f1 f2

    abs = nmap "valor absoluto" abs

    fromInteger v = AnInt (fromInteger v) (showt v)

    negate = nmap "negación" negate

    signum = nmap "signum" signum

instance Real Field where
    toRational = undefined

instance Ord Field where
    compare (AnInt n1 _) (AnInt n2 _) = compare n1 n2
    compare (AnInt n _) (ADouble d _) = compare (fromIntegral n) d
    compare (AnInt _ _) _ = LT

    compare (ADouble d _) (AnInt n _) = compare d (fromIntegral n)
    compare (ADouble d1 _) (ADouble d2 _) = compare d1 d2
    compare (ADouble _ _) _ = LT

    compare (AString s1) (AString s2) = compare s1 s2
    compare (AString _) (AnInt _ _) = GT
    compare (AString _) (ADouble _ _) = GT
    compare (AString _) _ = LT

    compare (AnError _ s1) (AnError _ s2) = compare s1 s2
    compare (AnError _ _) (AnInt _ _) = GT
    compare (AnError _ _) (ADouble _ _) = GT
    compare (AnError _ _) (AString _) = GT
    compare (AnError _ _) Empty = LT

    compare Empty Empty = EQ
    compare Empty _ = GT

instance Enum Field where
    toEnum v = AnInt v $ showt v

    fromEnum (AnInt n _) = n
    fromEnum _ = error "fromEnum de un no entero"

instance Integral Field where
    quot (AnInt n1 _) (AnInt n2 _) = toField $ quot n1 n2
    quot f1 f2 = typeError2 "división entera" f1 f2

    rem (AnInt n1 _) (AnInt n2 _) = toField $ rem n1 n2
    rem f1 f2 = typeError2 "resto" f1 f2

    div (AnInt n1 _) (AnInt n2 _) = toField $ div n1 n2
    div f1 f2 = typeError2 "división entera" f1 f2

    mod (AnInt n1 _) (AnInt n2 _) = toField $ mod n1 n2
    mod f1 f2 = typeError2 "resto" f1 f2

    quotRem (AnInt n1 _) (AnInt n2 _) = (toField q, toField r)
        where (q, r) = quotRem n1 n2
    quotRem f1 f2 = ( typeError2 "división entera" f1 f2
                    , typeError2 "resto" f1 f2)

    divMod (AnInt n1 _) (AnInt n2 _) = (toField d, toField m)
        where (d, m) = divMod n1 n2
    divMod f1 f2 = ( typeError2 "división entera" f1 f2
                    , typeError2 "resto" f1 f2)

    toInteger = toInteger . toInt

-- |Recover the Int from an integer expression or error
toInt :: Field -> Int
toInt (AnInt n _) = n
toInt f = error $ concat ["toInt de un "
                            , T.unpack (typeLabel $ typeOf f)
                            , " (", show f, ")"
                            ]

-- |Recover the Double from a double expression or error
toDouble :: Field -> Double
toDouble (ADouble d _) = d
toDouble f = error $ concat ["toDouble de un "
                            , T.unpack (typeLabel $ typeOf f)
                            , " (", show f, ")"
                            ]

instance Fractional Field where
    (AnInt n1 _) / (AnInt n2 _) = toField ((fromIntegral n1 / fromIntegral n2)::Double)
    (AnInt n _) / (ADouble d _) = toField $ fromIntegral n / d
    (ADouble d _) / (AnInt n _) = toField $ d / fromIntegral n
    (ADouble d1 _) / (ADouble d2 _) = toField $ d1/d2
    f1 / f2 = typeError2 "división" f1 f2

    recip (AnInt n _) = toField ((recip $ fromIntegral n) :: Double)
    recip (ADouble d _) = toField $ recip d
    recip f = typeError "recíproco" f

    fromRational v = toField (fromRational v :: Double)
