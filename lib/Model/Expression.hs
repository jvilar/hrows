{-# LANGUAGE DeriveFoldable
           , DeriveFunctor
           , DeriveTraversable
           , FlexibleContexts
           , FlexibleInstances
           , OverloadedStrings
           , TypeSynonymInstances
#-}

module Model.Expression ( -- *Types
                          Expression
                        , Node(..)
                        , Formula
                        , BinaryOp
                        , UnaryOp
                        , BinaryOpInfo(..)
                        , PBinaryOpInfo(..)
                        , UnaryOpInfo(..)
                        , Priority
                        , Associativity(..)
                        -- *Constructors for Expression
                        , mkPosition
                        , mkNamedPosition
                        , mkConstant
                        , mkUnary
                        , mkBinary
                        , mkPBinary
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
                   gshow (NamedPosition t) = "NamedPosition " ++ T.unpack t
                   gshow (Constant f) = "Constant " ++ show f
                   gshow (Unary u exp) = concat ["Unary ", show u, " (", exp, ")" ]
                   gshow (Binary b exp1 exp2) = concat ["Binary ", show b, " (", exp1,", ", exp2, ")" ]
                   gshow (PrefixBinary b exp1 exp2) = concat ["PrefixBinary ", show b, " (", exp1,", ", exp2, ")" ]
                   gshow (Ternary exp1 exp2 exp3) = concat ["Ternary ", " (", exp1, ", ", exp2, ", ", exp3, ")" ]
                   gshow (ErrorCheck exp1 exp2) = concat ["ErrorCheck ", " (", exp1, ", ", exp2, ")" ]
                   gshow (FromSource sName exp1 exp2 exp3) = concat["FromSource", " (", sName, ", ", exp1, ", ", exp2, ", ", exp3, ")"]
                   gshow (Cast t exp) = concat ["Cast ", show t, " (", exp, ")" ]
                   gshow (Error e) = T.unpack e

-- |A node in the AST for expressions
data Node a = Position Int -- ^A position in the row
            | NamedPosition Text -- ^The name of a position in the row
            | Constant Field -- ^A constant value
            | Unary UnaryOpInfo a -- ^Application of an unary operator
            | Binary BinaryOpInfo a a -- ^Application of a binary operator
            | PrefixBinary PBinaryOpInfo a a -- ^Aplication of a prefix binary operator (max or min) 
            | Ternary a a a -- ^Application of the ternary operator
            | ErrorCheck a a -- ^Check an error. If the left operator evaluates to an error return the evaluation of the right operator.
            | Cast FieldType a -- ^Application of cast
            | FromSource a a a a -- ^Recover from a source. Parameters: source, position in the row to compare with,
                                 -- position in the source, position in the source to get the value from 
            | Error Text -- ^An error
            deriving (Eq, Foldable, Functor, Show, Traversable)


mkPosition :: Int -> Expression
mkPosition = In . Position

mkNamedPosition :: Text -> Expression
mkNamedPosition = In . NamedPosition

mkConstant :: Field -> Expression
mkConstant = In . Constant

mkUnary :: UnaryOpInfo -> Expression -> Expression
mkUnary = (In .) . Unary

mkBinary :: BinaryOpInfo -> Expression -> Expression -> Expression
mkBinary i e e' = In $ Binary i e e'

mkPBinary :: PBinaryOpInfo -> Expression -> Expression -> Expression
mkPBinary i e e' = In $ PrefixBinary i e e'

mkTernary :: Expression -> Expression -> Expression -> Expression
mkTernary e e' e'' = In $ Ternary e e' e''

mkErrorCheck :: Expression -> Expression -> Expression
mkErrorCheck e e' = In $ ErrorCheck e e'

mkCast :: FieldType -> Expression -> Expression
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

-- |The information stored about a prefix binary operation.
data PBinaryOpInfo = PBinaryOpInfo { opPB :: BinaryOp
                                   , formulaPB :: Formula
                                   }

instance Show PBinaryOpInfo where
    show = T.unpack . formulaPB

instance Eq PBinaryOpInfo where
    a == b = formulaPB a == formulaPB b


toFormula :: Expression -> Formula
toFormula = para tf
    where tf :: RAlgebra Node Formula
          tf _ (Position p) = "$" `T.append` showt (p + 1)
          tf _ (NamedPosition n) | T.all isAlphaNum n = n
                                 | otherwise = T.concat ["@{", n, "}"]
          tf _ (Constant f) | typeOf f == TypeString = T.concat[ "\"", toString f, "\""]
                            | otherwise = toString f
          tf (In (Unary info e)) (Unary _ f) = formulaU info `T.append` parent 8 (prio e) f
          tf (In (Binary info e1 e2)) (Binary _ f1 f2) = let
                     (pe1, pe2) = case assocB info of
                                      LeftAssoc -> (prioB info, prioB info + 1)
                                      RightAssoc -> (prioB info + 1, prioB info)
                                      TrueAssoc -> (prioB info, prioB info)
                                      NoAssoc -> (prioB info + 1, prioB info + 1)
                     in T.concat [parent pe1 (prio e1) f1, formulaB info, parent pe2 (prio e2) f2]
          tf (In (PrefixBinary info _ _)) (PrefixBinary _ f1 f2) =
                    T.concat [formulaPB info, "(", f1, ", ", f2, ")"]
          tf _ (Cast ft f) = T.concat [typeOperator ft, "(", f, ")"]
          tf _ (ErrorCheck f1 f2) = T.concat ["(", f1, ") ?! (", f2, ")"]
          tf (In (Ternary e1 e2 e3)) (Ternary f1 f2 f3) = let
                f1' = parent 1 (prio e1) f1
                f2' = parent 0 (prio e2) f2
                f3' = parent 0 (prio e3) f3
              in T.concat [f1', "?", f2',  ":", f3']
          tf _ (Error s) = "Error: " `T.append` s

parent :: Priority -> Priority -> Text -> Text
parent p1 p2 s | p1 > p2 = T.concat ["(", s, ")"]
               | otherwise = s

prio :: Expression -> Priority
prio (In (Binary info _ _)) = prioB info
prio (In (Ternary _ _ _)) = 1
prio _ = 10


addCast :: FieldType -> Expression -> Expression
addCast ft exp@(In (Cast ft' e)) | ft == ft' = exp
                                 | otherwise = In (Cast ft e)
addCast ft exp = In (Cast ft exp)

-- |The positions referenced in the expression. They are sorted, without repetions and
--  only include those positions that refer to the current row, not to other data sources.
getPositions :: Expression -> [Int]
getPositions = cata gp
    where
        gp (Position n) = [n]
        gp (NamedPosition name) = error $ "Expresión con variable: " ++ T.unpack name
        gp (Constant _) = []
        gp (Unary _ ps) = ps
        gp (Binary _ ps1 ps2) = merge ps1 ps2
        gp (PrefixBinary _ ps1 ps2) = merge ps1 ps2
        gp (Cast _ ps) = ps
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

