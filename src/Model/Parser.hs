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
              Left err -> Error err
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
    then Ternary cond <$> (advance >> expression)
                      <*> (colon >> expression)
    else return cond

binaryLevel :: Parser Expression -> Parser (Maybe BinaryOpInfo) -> Parser Expression
binaryLevel nextLevel operator = do
    nl <- nextLevel
    mop <- operator
    case mop of
        Nothing -> return nl
        Just op -> Binary op nl <$> binaryLevel nextLevel operator

-- logical -> comparison (logOperator comparison)*
logical :: Parser Expression
logical = binaryLevel comparsion
            (match [ (AndT, BinaryOpInfo andField "&&" 2 TrueAssoc)
                   , (OrT, BinaryOpInfo orField "||" 2 TrueAssoc)
                   ]
            )

-- comparsion -> additive (compOperator additive)*
comparsion :: Parser Expression
comparsion = binaryLevel additive
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
    case t of
        IntT n -> advance >> (return . Constant $ toField n)
        DoubleT d -> advance >> (return . Constant $ toField d)
        StringT s -> advance >> (return . Constant $ toField s)
        PositionT n -> advance >> return (Position $ n - 1)
        NameT s -> advance >> return (NamedPosition s)
        CastT ft -> advance >> parenthesized >>= return . Cast ft
        OpenT -> advance >> (expression <* close)
        _ -> throwError $ "Error en " ++ show t ++ ", esperaba un comienzo de expresión"

parenthesized :: Parser Expression
parenthesized = open >> expression <* close

open :: Parser ()
open = expect OpenT "un paréntesis abierto"

close :: Parser ()
close = expect CloseT "un paréntesis cerrado"

colon :: Parser ()
colon = expect ColonT "dos puntos"
