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

combine :: Expression -> [(BinaryOp, Expression)] -> Expression
combine = foldl' (\t1 (op, t2) -> Binary op t1 t2)

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
        PositionT n -> advance >> return (Position n)
        NameT s -> advance >> return (NamedPosition s)
        OpenT -> advance >> (expression <* close)
        _ -> throwError $ "Error en " ++ show t ++ ", esperaba un comienzo de expresión"

isAdditive :: Parser Bool
isAdditive = do
    t <- current
    return $ case t of
                 AddT -> True
                 SubT -> True
                 _ -> False

additive :: Parser BinaryOp
additive = do
    t <- current
    case t of
        AddT -> advance >> return (+)
        SubT -> advance >> return (-)
        _ -> throwError $ "Error en " ++ show t ++ ", esperaba suma o resta"

isMultiplicative :: Parser Bool
isMultiplicative = do
    t <- current
    return $ case t of
                 MultT -> True
                 DivT -> True
                 _ -> False

multiplicative :: Parser BinaryOp
multiplicative = do
    t <- current
    case t of
        MultT -> advance >> return (*)
        DivT -> advance >> return (/)
        _ -> throwError $ "Error en " ++ show t ++ ", esperaba producto o división"

close :: Parser ()
close = do
    t <- current
    case t of
        CloseT -> advance
        _ -> throwError $ "Error en " ++ show t ++ ", esperaba un paréntesis cerrado"
