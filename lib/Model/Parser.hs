{-# LANGUAGE TupleSections #-}

module Model.Parser ( parse
                    ) where

import Control.Monad(unless)
import Control.Monad.Except(ExceptT, runExceptT, throwError)
import Control.Monad.State.Strict(evalState, gets, State, modify)
import Control.Monad.Trans(lift)
import Data.List(foldl')
import Data.Maybe(isNothing)

import Model.Lexer
import Model.Expression

type Parser = ExceptT String (State [Token])

many :: Parser (Maybe a) -> Parser [a]
many p = do
    mx <- p
    case mx of
        Just x -> (x:) <$> many p
        Nothing -> return []

current :: Parser Token
current = lift $ gets head

advance :: Parser ()
advance = lift $ modify tail

check :: Token -> Parser Bool
check t = (== t) <$> current

expect :: Token -> String -> Parser ()
expect t message = do
    c <- current
    unless (t == c) $ throwError $ "Error en " ++ show c ++ ", esperaba " ++ message
    advance

match :: [(Token, a)] -> Parser (Maybe a)
match l = do
    t <- current
    let r = lookup t l
    unless (isNothing r) advance
    return r

parse :: Formula -> Expression
parse s = case evalState (runExceptT (expression <* eof)) $ tokenize s of
              Left err -> mkErrorExpr err
              Right e -> e

eof :: Parser ()
eof = do
    t <- current
    case t of
        EOFT -> return ()
        _ -> throwError $ "Error en " ++ show t ++ ", esperaba el fin de la expresión"

-- expression -> logical (QuestionMarkT expression ColonT expression)?
expression :: Parser Expression
expression = do
    cond <- logical
    q <- check QuestionMarkT
    if q
    then mkTernary cond <$> (advance >> expression)
                        <*> (colon >> expression)
    else return cond

binaryLevel :: Parser Expression -> Parser (Maybe BinaryOpInfo) -> Parser Expression
binaryLevel nextLevel operator = nextLevel >>= go
    where go left = do
            mop <- operator
            case mop of
              Nothing -> return left
              Just op -> (mkBinary op left <$> nextLevel) >>= go

-- logical -> conjunction (orOperator conjunction)*
logical :: Parser Expression
logical = binaryLevel conjunction
            (match [ (OrT, BinaryOpInfo orField "||" 2 TrueAssoc) ])

-- conjuntion -> comparsion (andOperator comparison)*
conjunction :: Parser Expression
conjunction = binaryLevel comparison
            (match [ (AndT, BinaryOpInfo andField "&&" 2 TrueAssoc) ])


-- comparsion -> additive (compOperator additive)*
comparison :: Parser Expression
comparison = binaryLevel additive
             ( match [ (EqualT, BinaryOpInfo (compareField (==)) "==" 3 LeftAssoc)
                     , (NotEqualT, BinaryOpInfo (compareField (/=)) "!=" 3 LeftAssoc)
                     , (LessThanT, BinaryOpInfo (compareField (<)) "<" 3 LeftAssoc)
                     , (LessOrEqualT, BinaryOpInfo (compareField (<=)) "<=" 3 LeftAssoc)
                     , (GreaterThanT, BinaryOpInfo (compareField (>)) ">" 3 LeftAssoc)
                     , (GreaterOrEqualT, BinaryOpInfo (compareField (>=)) ">=" 3 LeftAssoc)
                     ]
             )

-- additive -> multiplicative (addOperator multiplicative)*
additive :: Parser Expression
additive = binaryLevel multiplicative
           ( match [ (AddT, BinaryOpInfo (+) "+" 4 TrueAssoc)
                   , (SubT, BinaryOpInfo (-) "-" 4 LeftAssoc)
                   ]
           )

-- multiplicative -> base (multOperator base)*
multiplicative :: Parser Expression
multiplicative = binaryLevel base
                 ( match [ (MultT, BinaryOpInfo (*) "*" 5 TrueAssoc)
                         , (DivT, BinaryOpInfo (/) "/" 5 LeftAssoc)
                         ]
                 )

-- base -> IntT | DoubleT | StringT | PositionT | NameT | CastT parenthesized | OpenT expression CloseT
base :: Parser Expression
base = do
    t <- current
    advance >> case t of
        IntT n -> return . mkConstant $ toField n
        DoubleT d -> return . mkConstant $ toField d
        StringT s -> return . mkConstant $ toField s
        PositionT n -> return (mkPosition $ n - 1)
        NameT s -> return $ mkNamedPosition s
        CastT ft -> mkCast ft <$> parenthesized
        OpenT -> expression <* close
        _ -> throwError $ "Error en " ++ show t ++ ", esperaba un comienzo de expresión"

parenthesized :: Parser Expression
parenthesized = open >> expression <* close

open :: Parser ()
open = expect OpenT "un paréntesis abierto"

close :: Parser ()
close = expect CloseT "un paréntesis cerrado"

colon :: Parser ()
colon = expect ColonT "dos puntos"
