module Model.Parser ( parse
                    ) where

import Control.Monad.Except(ExceptT, runExceptT, throwError)
import Control.Monad.State.Strict(evalState, gets, State, modify)
import Control.Monad.Trans(lift)
import Data.List(foldl')

import Model.Lexer
import Model.Expression


type Parser = ExceptT String (State [Token])

many :: Parser Bool -> Parser a -> Parser [a]
many cond body = do
    c <- cond
    if c
    then (:) <$> body <*> many cond body
    else return []

current :: Parser Token
current = lift $ gets head

advance :: Parser ()
advance = lift $ modify tail

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

-- expression -> term (additive term)*
expression :: Parser Expression
expression = combine
             <$> term
             <*> many isAdditive ((,) <$> additive <*> term)

data BinaryOpInfo = BinaryOpInfo BinaryOp Priority String
data UnaryOpInfo = UnaryOpInfo UnaryOp String

combine :: Expression -> [(BinaryOpInfo, Expression)] -> Expression
combine = foldl' (\t1 (BinaryOpInfo op p str, t2) -> Binary p str op t1 t2)

term :: Parser Expression
term = combine
       <$> base
       <*> many isMultiplicative ((,) <$> multiplicative <*> base)

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

isAdditive :: Parser Bool
isAdditive = do
    t <- current
    return $ case t of
                 AddT -> True
                 SubT -> True
                 _ -> False

match :: [(Token, a)] -> String -> Parser a
match l message = do
    t <- current
    case lookup t l of
        Nothing -> throwError $ "Error en " ++ show t ++ ", esperaba " ++ message
        Just a -> advance >> return a

additive :: Parser BinaryOpInfo
additive = match [(AddT, BinaryOpInfo (+) 4 "+"), (SubT, BinaryOpInfo (-) 4 "-")] "una suma o resta"

isMultiplicative :: Parser Bool
isMultiplicative = do
    t <- current
    return $ case t of
                 MultT -> True
                 DivT -> True
                 _ -> False

multiplicative :: Parser BinaryOpInfo
multiplicative = match [(MultT, BinaryOpInfo (*) 5 "*"), (DivT, BinaryOpInfo (/) 5 "/")] "un producto o una división"

open :: Parser ()
open = match [(OpenT, ())] "un paréntesis abierto"

close :: Parser ()
close = match [(CloseT, ())] "un paréntesis cerrado"
