{-# LANGUAGE OverloadedStrings #-}

module Model.Expression.Parser ( parse ) where

import Control.Monad(unless)
import Control.Monad.Except(ExceptT, runExceptT, throwError)
import Control.Monad.State.Strict(evalState, gets, State, modify)
import Control.Monad.Trans(lift)
import Data.Maybe(isNothing)
import Data.Text(Text)
import qualified Data.Text as T
import TextShow(TextShow(..))

import Model.Expression.Lexer
import Model.Expression

type Parser = ExceptT Text (State [Token])

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

expect :: Token -> Text -> Parser ()
expect t message = do
    c <- current
    unless (t == c) $ throwError $ T.concat ["Error en ", showt c, ", esperaba ", message]
    advance

expectName :: Text -> Parser Expression
expectName message = do
    c <- current
    case c of
      NameT s -> do
           advance
           return $ mkNamedPosition (T.pack s)
      _ -> throwError $ T.concat ["Error en ", showt c, ", se esperaba ", message]

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
        _ -> throwError $ T.concat ["Error en ", showt t, ", esperaba el fin de la expresión"]

-- expression -> logical (QuestionMarkT expression ColonT expression)?
expression :: Parser Expression
expression = do
    eCheck <- errorCheck
    q <- check QuestionMarkT
    if q
    then mkTernary eCheck <$> (advance >> expression)
                        <*> (colon >> expression)
    else return eCheck

-- errorCheck -> logical (IsErrorT errorCheck)?
errorCheck :: Parser Expression
errorCheck = do
    left <- logical
    q <- check IsErrorT
    if q
    then mkErrorCheck left <$> (advance >> errorCheck)
    else return left

binaryLevel :: Parser Expression -> Parser (Maybe BinaryOpInfo) -> Parser Expression
binaryLevel nextLevel operator = nextLevel >>= go
    where go left = do
            mop <- operator
            case mop of
              Nothing -> return left
              Just op -> nextLevel >>= go . mkBinary op left

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

-- base -> IntT | DoubleT | StringT | PositionT | NameT name | NotT base
--         | CastT parenthesized | OpenT expression CloseT
--         | MaxT OpenT expression CommaT expression CloseT
--         | MinT OpenT expression CommaT expression CloseT
base :: Parser Expression
base = do
    t <- current
    advance >> case t of
        IntT n -> return . mkConstant $ toField n
        DoubleT d -> return . mkConstant $ toField d
        StringT s -> return . mkConstant $ toField $ T.pack s
        PositionT n -> return (mkPosition $ n - 1)
        NameT s -> name s
        NotT -> mkUnary (UnaryOpInfo notField "!") <$> base
        CastT ft -> mkCast ft <$> parenthesized
        OpenT -> expression <* close
        MaxT -> maxMin $ PBinaryOpInfo maxField "max"
        MinT -> maxMin $ PBinaryOpInfo minField "min"
        _ -> throwError $ T.concat ["Error en ", showt t, ", esperaba un comienzo de expresión"]

-- name --> (AtT NameT ArrowT NameT EqualT NameT)?
name :: String -> Parser Expression
name s = do
        let pos = mkNamedPosition (T.pack s)
        at <- check AtT
        if at
        then do
                advance
                source <- expectName "el nombre de la fuente"
                expect ArrowT "una flecha"
                fHere <- expression
                expect DoubleArrowT "una flecha doble"
                fThere <- expression
                return $ mkFromSource source fHere fThere pos 
        else return pos
parenthesized :: Parser Expression
parenthesized = open >> expression <* close

open :: Parser ()
open = expect OpenT "un paréntesis abierto"

close :: Parser ()
close = expect CloseT "un paréntesis cerrado"

colon :: Parser ()
colon = expect ColonT "dos puntos"

maxMin :: PBinaryOpInfo -> Parser Expression
maxMin op = do
              open
              left <- expression
              expect CommaT "una coma"
              right <- expression
              close
              return $ mkPBinary op left right
