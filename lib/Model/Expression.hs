{-# LANGUAGE DeriveTraversable, FlexibleContexts, FlexibleInstances, OverloadedStrings #-}

module Model.Expression ( -- *Types
                          Expression
                        , Node(..)
                        , Formula
                        , BinaryOp
                        , UnaryOp
                        , BinaryOpInfo(..)
                        , PrefixOpInfo(..)
                        , UnaryOpInfo(..)
                        , Priority
                        , Associativity(..)
                        -- *Constructors for Expression
                        , mkPosition
                        , mkNamedPosition
                        , mkKnownNamedPosition
                        , mkConstant
                        , mkUnary
                        , mkBinary
                        , mkPrefix
                        , mkTernary
                        , mkErrorCheck
                        , mkCast
                        , mkFromSource
                        , mkErrorExpr
                        -- *Querying
                        , getPositions
                        -- *Other functions
                        , addCast
                        , toFormula
                        -- *Rexported
                        , module Model.Field
                        ) where

import Data.Char(isAlphaNum)
import Data.List(intercalate)
import Data.Text(Text)
import qualified Data.Text as T
import TextShow(TextShow(showt))
import Model.Field
import Model.Expression.RecursionSchemas

-- |The Formula is the expression as written by the user.
type Formula = Text

-- |The priority of an operator
type Priority = Int

-- |The associativity of a binary operator
data Associativity = LeftAssoc | RightAssoc | TrueAssoc | NoAssoc

-- |The Expression is the internal representation of the Formula.
type Expression = Fix Node

instance Eq Expression where
  (In n) == (In m) = n == m

instance Show Expression where
    show = cata gshow
             where gshow (Position i) = "Position " ++ show i
                   gshow (NamedPosition t n) = concat ["NamedPosition ", show t
                                                      , " ", show n ]
                   gshow (Constant f) = "Constant " ++ show f
                   gshow (Unary u ex) = concat ["Unary ", show u, " (", ex, ")" ]
                   gshow (Binary b ex1 ex2) = concat ["Binary ", show b, " (", ex1,", ", ex2, ")" ]
                   gshow (Prefix b exs) = concat ["Prefix", show b, " ("] ++ intercalate ", " exs ++ ")"
                   gshow (Ternary ex1 ex2 ex3) = concat ["Ternary ", " (", ex1, ", ", ex2, ", ", ex3, ")" ]
                   gshow (ErrorCheck ex1 ex2) = concat ["ErrorCheck ", " (", ex1, ", ", ex2, ")" ]
                   gshow (FromSource sName ex1 ex2 ex3) = concat["FromSource", " (", sName, ", ", ex1, ", ", ex2, ", ", ex3, ")"]
                   gshow (Cast t exs) = concat ["Cast ", show t, " ("] ++ intercalate ", " exs ++ ")"
                   gshow (Error e) = T.unpack e

-- |A node in the AST for expressions
data Node a = Position Int -- ^A position in the row
            | NamedPosition Text (Maybe Int) -- ^The name of a position in the row and its index
            | Constant Field -- ^A constant value
            | Unary UnaryOpInfo a -- ^Application of an unary operator
            | Binary BinaryOpInfo a a -- ^Application of a binary operator
            | Prefix PrefixOpInfo [a] -- ^Aplication of a prefix operator (max or min) 
            | Ternary a a a -- ^Application of the ternary operator
            | ErrorCheck a a -- ^Check an error. If the left operator evaluates to an error return the evaluation of the right operator.
            | Cast FieldType [a] -- ^Application of cast
            | FromSource a a a a -- ^Recover from a source. Parameters: source, position in the row to compare with,
                                 -- position in the source, position in the source to get the value from 
            | Error Text -- ^An error
            deriving (Eq, Foldable, Functor, Show, Traversable)


mkPosition :: Int -> Expression
mkPosition = In . Position

mkNamedPosition :: Text -> Expression
mkNamedPosition t = In $ NamedPosition t Nothing

mkKnownNamedPosition :: Text -> Int -> Expression
mkKnownNamedPosition t n = In $ NamedPosition t (Just n)

mkConstant :: Field -> Expression
mkConstant = In . Constant

mkUnary :: UnaryOpInfo -> Expression -> Expression
mkUnary = (In .) . Unary

mkBinary :: BinaryOpInfo -> Expression -> Expression -> Expression
mkBinary i e e' = In $ Binary i e e'

mkPrefix :: PrefixOpInfo -> [Expression] -> Expression
mkPrefix i es = In $ Prefix i es

mkTernary :: Expression -> Expression -> Expression -> Expression
mkTernary e e' e'' = In $ Ternary e e' e''

mkErrorCheck :: Expression -> Expression -> Expression
mkErrorCheck e e' = In $ ErrorCheck e e'

mkCast :: FieldType -> [Expression] -> Expression
mkCast = (In .) . Cast

mkFromSource :: Expression -> Expression -> Expression -> Expression -> Expression
mkFromSource name e e' e'' = In $ FromSource name e e' e''

mkErrorExpr :: Text -> Expression
mkErrorExpr = In . Error

type UnaryOp = Field -> Field

-- |The information stored about a unary operation.
data UnaryOpInfo = UnaryOpInfo { opU :: UnaryOp
                               , formulaU :: Formula
                               }

instance Show UnaryOpInfo where
    show = T.unpack . formulaU

instance Eq UnaryOpInfo where
    a == b = formulaU a == formulaU b

type BinaryOp = Field -> Field -> Field

-- |The information stored about a binary operation.
data BinaryOpInfo = BinaryOpInfo { opB :: BinaryOp
                                 , formulaB :: Formula
                                 , prioB :: Priority
                                 , assocB :: Associativity
                                 }

instance Show BinaryOpInfo where
    show = T.unpack . formulaB

instance Eq BinaryOpInfo where
    a == b = formulaB a == formulaB b

type PrefixOp = [Field] -> Field

-- |The information stored about a prefix operation.
data PrefixOpInfo = PrefixOpInfo { opP :: PrefixOp
                                 , formulaP :: Formula
                                 }

instance Show PrefixOpInfo where
    show = T.unpack . formulaP

instance Eq PrefixOpInfo where
    a == b = formulaP a == formulaP b


toFormula :: Expression -> Formula
toFormula = para tf
    where tf :: RAlgebra Node Formula
          tf (Position p) = "$" `T.append` showt (p + 1)
          tf (NamedPosition n _ ) | T.all isAlphaNum n = n
                                  | otherwise = T.concat ["@{", n, "}"]
          tf (Constant f) | typeOf f == TypeString = T.concat[ "\"", toString f, "\""]
                          | otherwise = toString f
          tf (Unary info (e, f)) = formulaU info `T.append` parent 8 (prio e) f
          tf (Binary info (e1, f1) (e2, f2)) = let
                     (pe1, pe2) = case assocB info of
                                      LeftAssoc -> (prioB info, prioB info + 1)
                                      RightAssoc -> (prioB info + 1, prioB info)
                                      TrueAssoc -> (prioB info, prioB info)
                                      NoAssoc -> (prioB info + 1, prioB info + 1)
                     in T.concat [parent pe1 (prio e1) f1, formulaB info, parent pe2 (prio e2) f2]
          tf (Prefix info fs) =
                    T.concat $ [formulaP info, "(", T.intercalate ", " $ map snd fs, ")"]
          tf (Cast ft fs) = T.concat [typeOperator ft, "(", T.intercalate ", " $ map snd fs, ")"]
          tf (ErrorCheck (_, f1) (_, f2)) = T.concat ["(", f1, ") ?! (", f2, ")"]
          tf (Ternary (e1, f1) (e2, f2) (e3, f3)) = let
                f1' = parent 1 (prio e1) f1
                f2' = parent 0 (prio e2) f2
                f3' = parent 0 (prio e3) f3
              in T.concat [f1', "?", f2',  ":", f3']
          tf (FromSource (e1, f1) (e2, f2) (e3, f3) (e4, f4)) = let
                f1' = parent 1 (prio e1) f1
                f2' = parent 0 (prio e2) f2
                f3' = parent 0 (prio e3) f3
                f4' = parent 0 (prio e4) f4
              in T.concat [f4', "@", f1', "<-", f2', "<->", f3']
          tf (Error s) = "Error: " `T.append` s

parent :: Priority -> Priority -> Text -> Text
parent p1 p2 s | p1 > p2 = T.concat ["(", s, ")"]
               | otherwise = s

prio :: Expression -> Priority
prio (In (Binary info _ _)) = prioB info
prio (In Ternary {}) = 1
prio _ = 10


addCast :: FieldType -> Expression -> Expression
addCast ft ex@(In (Cast ft' exs)) | ft == ft' = ex
                                  | otherwise = In (Cast ft exs)
addCast ft ex = In (Cast ft [ex])

-- |The positions referenced in the expression. They are sorted, without repetions and
--  only include those positions that refer to the current row, not to other data sources.
getPositions :: Expression -> [Int]
getPositions = cata gp
    where
        gp (Position n) = [n]
        gp (NamedPosition _ Nothing) = []
        gp (NamedPosition _ (Just n)) = [n]
        gp (Constant _) = []
        gp (Unary _ ps) = ps
        gp (Binary _ ps1 ps2) = merge ps1 ps2
        gp (Prefix _ pss) = foldr merge [] pss
        gp (Cast _ pss) = foldr merge [] pss
        gp (Ternary ps1 ps2 ps3) = ps1 `merge` ps2 `merge` ps3
        gp (ErrorCheck ps1 ps2) = merge ps1 ps2
        gp (FromSource _ ps _ _) = ps
        gp (Error _) = []

merge :: [Int] -> [Int] -> [Int]
merge [] l = l
merge l@(_:_) [] = l
merge (x:xs) (y:ys) = case compare x y of
                          LT -> x : merge xs (y:ys)
                          EQ -> x : merge xs ys
                          GT -> y : merge (x:xs) ys

