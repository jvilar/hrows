{-# LANGUAGE DeriveFoldable
           , DeriveFunctor
           , DeriveTraversable
           , FlexibleContexts
           , FlexibleInstances
           , TypeSynonymInstances
#-}

module Model.Expression ( -- *Types
                          Expression
                        , Formula
                        , BinaryOp
                        , UnaryOp
                        , BinaryOpInfo(..)
                        , UnaryOpInfo(..)
                        , Priority
                        , Associativity(..)
                        -- *Constructors for Expression
                        , mkPosition
                        , mkNamedPosition
                        , mkConstant
                        , mkUnary
                        , mkBinary
                        , mkTernary
                        , mkCast
                        , mkErrorExpr
                        -- *Other functions
                        , addCast
                        , evaluate
                        , eliminateNames
                        , translatePositions
                        , translateNames
                        , getPositions
                        , toFormula
                        , module Model.Field
                        ) where

import Control.Arrow(second, (>>>), (<<<))
import Control.Monad(when)
import Control.Monad.Reader(Reader, ask, runReader)
import Control.Monad.Writer(Writer, tell, runWriter)
import Data.Char(isAlphaNum)
import Data.Function((&))
import Data.List(foldr1, elemIndex)
import Data.Maybe(fromMaybe)
import Data.Monoid(Any(..))
import Model.Field
import Model.Row

-- |The Formula is the expression as written by the user.
type Formula = String

-- |The priority of an operator
type Priority = Int

-- |The associativity of a binary operator
data Associativity = LeftAssoc | RightAssoc | TrueAssoc | NoAssoc

data WithNames
data WithNoNames

-- |The Expression is the internal representation of the Formula.
type Expression = Fix Node

instance Show Expression where
    show = cata show

newtype Fix t = In { out :: t (Fix t) }

data Node a = Position Int
            | NamedPosition String
            | Constant Field
            | Unary UnaryOpInfo a
            | Binary BinaryOpInfo a a
            | Ternary a a a
            | Cast FieldType a
            | Error String
            deriving (Foldable, Functor, Show, Traversable)


mkPosition :: Int -> Expression
mkPosition = In . Position

mkNamedPosition :: String -> Expression
mkNamedPosition = In . NamedPosition

mkConstant :: Field -> Expression
mkConstant = In . Constant

mkUnary :: UnaryOpInfo -> Expression -> Expression
mkUnary = (In .) . Unary

mkBinary :: BinaryOpInfo -> Expression -> Expression -> Expression
mkBinary i e e' = In $ Binary i e e'

mkTernary :: Expression -> Expression -> Expression -> Expression
mkTernary e e' e'' = In $ Ternary e e' e''

mkCast :: FieldType -> Expression -> Expression
mkCast = (In .) . Cast

mkErrorExpr :: String -> Expression
mkErrorExpr = In . Error

type UnaryOp = Field -> Field

-- |The information stored about a unary operation.
data UnaryOpInfo = UnaryOpInfo { opU :: UnaryOp
                               , formulaU :: Formula
                               }

instance Show UnaryOpInfo where
    show = formulaU

type BinaryOp = Field -> Field -> Field

-- |The information stored about a binary operation.
data BinaryOpInfo = BinaryOpInfo { opB :: BinaryOp
                                 , formulaB :: Formula
                                 , prioB :: Priority
                                 , assocB :: Associativity
                                 }

instance Show BinaryOpInfo where
    show = formulaB

type Algebra f a = f a -> a

bottomUp :: Functor a => (Fix a -> Fix a) -> Fix a -> Fix a
bottomUp f = out >>> fmap (bottomUp f) >>> In >>> f

bottomUpM :: (Monad m, Traversable a) => (Fix a -> m (Fix a)) -> Fix a -> m (Fix a)
-- bottomUpM f = out >>> fmap (bottomUp f) >>> In >>> f
bottomUpM f v = mapM (bottomUpM f) (out v) >>= f . In

topDown  :: Functor a => (Fix a -> Fix a) -> Fix a -> Fix a
topDown f = In <<< fmap (topDown f) <<< out <<< f

cata :: Functor f => Algebra f a -> Fix f -> a
cata f = out >>> fmap (cata f) >>> f

type AlgebraM m f a = f a -> m a

cataM :: (Traversable f, Monad m) => AlgebraM m f a -> Fix f -> m a
cataM f v = mapM (cataM f) (out v) >>= f

type RAlgebra f a = Fix f -> f a -> a

para :: Functor f => RAlgebra f a -> Fix f -> a
para rAlg t = out t & fmap (para rAlg) & rAlg t

evaluate :: Row -> Expression -> Field
evaluate r exp = runReader (eval exp) r

toFormula :: Expression -> Formula
toFormula = para tf
    where tf :: RAlgebra Node Formula
          tf _ (Position p) = "$" ++ show (p + 1)
          tf _ (NamedPosition n) | all isAlphaNum n = n
                                 | otherwise = "@{" ++ n ++ "}"
          tf _ (Constant f) | typeOf f == TypeString = '"' : toString f ++ "\""
                            | otherwise = toString f
          tf (In (Unary info e)) (Unary _ f) = formulaU info ++ parent 8 (prio e) f
          tf (In (Binary info e1 e2)) (Binary _ f1 f2) = let
                     (pe1, pe2) = case assocB info of
                                      LeftAssoc -> (prioB info, prioB info + 1)
                                      RightAssoc -> (prioB info + 1, prioB info)
                                      TrueAssoc -> (prioB info, prioB info)
                                      NoAssoc -> (prioB info + 1, prioB info + 1)
                     in parent pe1 (prio e1) f1 ++ formulaB info ++ parent pe2 (prio e2) f2
          tf _ (Cast ft f) = typeOperator ft ++ "(" ++ f ++ ")"
          tf (In (Ternary e1 e2 e3)) (Ternary f1 f2 f3) = let
                f1' = parent 1 (prio e1) f1
                f2' = parent 0 (prio e2) f2
                f3' = parent 0 (prio e3) f3
              in f1' ++ "?" ++ f2' ++ ":" ++ f3'
          tf _ (Error s) = "Error: " ++ s

parent :: Priority -> Priority -> String -> String
parent p1 p2 s | p1 > p2 = "(" ++ s ++ ")"
               | otherwise = s

prio :: Expression -> Priority
prio (In (Binary info _ _)) = prioB info
prio (In (Ternary _ _ _)) = 1
prio _ = 10

type Eval = Reader Row
eval :: Expression -> Eval Field
eval = cataM ev
    where
      ev (Position n) = evalIndex n
      ev (NamedPosition name) = return . mkError $ "Expresión con variable: " ++ name
      ev (Constant f) = return f
      ev (Unary info v) = return $ opU info v
      ev (Binary info v1 v2) = return $ opB info v1 v2
      ev (Cast ft v) = return $ convert ft v
      ev (Ternary v1 v2 v3) = return $ ternary v1 v2 v3
      ev (Error m) = return $ mkError m

evalIndex :: Int -> Eval Field
evalIndex n = do
    r <- ask
    return $ if 0 <= n && n < length r
             then r !! n
             else mkError $ "Índice erróneo " ++ show (n + 1)

eliminateNames :: [String] -> Expression -> Expression
eliminateNames fnames = bottomUp noNames
    where noNames (In (NamedPosition name)) = In $ case elemIndex name fnames of
                                                 Nothing -> Error $ "Mal nombre de campo: " ++ name
                                                 Just i -> Position i
          noNames n = n

type Changed = Bool

-- |Changes the absolute references according to the list of new positions.
-- Returns True if any position changed.
translatePositions :: [Int] -> Expression -> (Expression, Changed)
translatePositions newPos = second getAny . runWriter . bottomUpM tPos
    where tPos :: Expression -> Writer Any Expression
          tPos (In (Position n)) = do
              let n' = newPos !! n
              when (n' /= n) $ tell (Any True)
              return . In $ Position n'
          tPos e = return e

translateNames :: [(String, String)] -> Expression -> (Expression, Changed)
translateNames newNames = second getAny . runWriter . bottomUpM tNames
    where tNames (In (NamedPosition name)) = do
              let name' = fromMaybe name (lookup name newNames)
              when (name' /= name) $ tell (Any True)
              return . In $ NamedPosition name'
          tNames e = return e

getPositions :: Expression -> [Int]
getPositions = cata gp
    where
        gp (Position n) = [n]
        gp (NamedPosition name) = error $ "Expresión con variable: " ++ name
        gp (Constant f) = []
        gp (Unary _ ps) = ps
        gp (Binary _ ps1 ps2) = merge ps1 ps2
        gp (Cast _ ps) = ps
        gp (Ternary ps1 ps2 ps3) = ps1 `merge` ps2 `merge` ps3
        gp (Error _) = []

merge :: [Int] -> [Int] -> [Int]
merge [] l = l
merge l@(_:_) [] = l
merge (x:xs) (y:ys) = case compare x y of
                          LT -> x : merge xs (y:ys)
                          EQ -> x : merge xs ys
                          GT -> y : merge (x:xs) ys

addCast :: FieldType -> Expression -> Expression
addCast ft exp@(In (Cast ft' e)) | ft == ft' = exp
                                 | otherwise = In (Cast ft e)
addCast ft exp = In (Cast ft exp)

