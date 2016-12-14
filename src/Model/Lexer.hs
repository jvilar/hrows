module Model.Lexer ( Token (..)
                   , tokenize
                   ) where

import Control.Arrow(first)
import Control.Monad(void, when)
import Control.Monad.State.Strict(evalStateT, get, gets, StateT, modify, put)
import Control.Monad.Writer(execWriter, tell, Writer)
import Data.Char(isAlpha, isAlphaNum, isDigit, isSpace)

import Model.Field

import Prelude hiding (lex)

data Token = IntT Int
           | DoubleT Double
           | StringT String
           | PositionT Int
           | NameT String
           | AddT
           | SubT
           | MultT
           | DivT
           | OpenT
           | CloseT
           | CastT FieldType
           | EOFT
           | ErrorT String
           deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize inp = execWriter $ evalStateT tokenizer ([], inp)

type Lexeme = String
type Input = String

type Tokenizer = StateT (Lexeme, Input) (Writer [Token])

peek :: Tokenizer (Maybe Char)
peek = do
    inp <- gets snd
    return $ case inp of
        [] -> Nothing
        c:_ -> Just c

pop :: Tokenizer ()
pop = do
    (c:lex, inp) <- get
    put (lex, c:inp)

next :: Tokenizer (Maybe Char)
next = do
    (lex, inp) <- get
    case inp of
        [] -> return Nothing
        c:cs -> do
            put (c:lex, cs)
            return $ Just c

lexeme :: Tokenizer Lexeme
lexeme = gets $ reverse . fst

emit :: Token -> Tokenizer ()
emit t = do
    tell [t]
    inp <- gets snd
    put ([], inp)

emitl :: (Lexeme -> Token) -> Tokenizer ()
emitl f = do
    (lex, inp) <- get
    tell [f $ reverse lex]
    put ([], inp)

omit :: Tokenizer ()
omit = modify (first $ const [])

select :: Monad m => [(Bool, m ())] -> m ()
select [] = return ()
select ((False, _) : ms) = select ms
select ((True, m) : _) = m

with :: Tokenizer (Maybe Char) -> Tokenizer a -> (Char -> Tokenizer a) -> Tokenizer a
with read onNothing onJust = read >>= maybe onNothing onJust

many :: (Char -> Bool) -> Tokenizer ()
many cond = ifChar cond
            (many cond)
            (return ())

many1 :: (Char -> Bool) -> Tokenizer() -> Tokenizer() -> Tokenizer()
many1 cond onError onSuccess = ifChar cond
                               (many cond >> onSuccess)
                               onError

ifChar :: (Char -> Bool) -> Tokenizer() -> Tokenizer() -> Tokenizer()
ifChar cond onSuccess onError = with peek
    onError
    (\c -> if cond c
           then next >> onSuccess
           else onError
    )

optionalChar :: (Char -> Bool) -> Tokenizer()
optionalChar cond = peek >>=
                    maybe (return ())
                          (\c -> when (cond c) (void next))

needChar :: (Char -> Bool) -> Tokenizer() -> Tokenizer()
needChar cond onSuccess = ifChar cond onSuccess (emitl ErrorT)

tokenizer :: Tokenizer ()
tokenizer =
    with next
        (emit EOFT)
        (\c -> do
                select [ (isDigit c, number)
                       , (isAlpha c, shortNamed)
                       , (c == '"' , string)
                       , (c == '$' , position)
                       , (c == '@' , named)
                       , (c == '+' , emit AddT)
                       , (c == '-' , emit SubT)
                       , (c == '*' , emit MultT)
                       , (c == '/' , emit DivT)
                       , (c == '(' , emit OpenT)
                       , (c == ')' , emit CloseT)
                       , (isSpace c, omit)
                       , (otherwise , emitl ErrorT)
                       ]
                tokenizer
        )

number :: Tokenizer ()
number = do
    many isDigit
    ifChar (== '.') afterPoint
       (ifChar (== 'e') (afterE (IntT . read))
          (emitl $ IntT . read)
       )

afterPoint :: Tokenizer ()
afterPoint = many1 isDigit
                   (emitl ErrorT)
                   (ifChar (== 'e')
                       (afterE (DoubleT . read))
                       (emitl $ DoubleT . read)
                   )

afterE :: (String -> Token) -> Tokenizer ()
afterE conv =  with peek
               (pop >> emitl conv)
               (\c -> do
                       let sign = c == '-' || c == '+'
                       when sign (void next)
                       many1 isDigit
                         (do
                                 when sign pop
                                 pop
                                 emitl conv)
                         (emitl $ DoubleT . read)
               )

string :: Tokenizer ()
string = do
           many (/= '"')
           ifChar (== '"')
             (emitl $ StringT . tail . init)
             (emitl ErrorT)

position :: Tokenizer ()
position = many1 isDigit
             (emitl ErrorT)
             (emitl $ PositionT . read . tail)

named :: Tokenizer ()
named = needChar (== '{')
        ( many1 (/= '}')
          (emitl ErrorT)
          (needChar (== '}')
           (emitl $ NameT . init . drop 2)
          )
        )

reservedWords :: [(String, Token)]
reservedWords = [(typeOperator t, CastT t) | t <- [TypeString, TypeInt, TypeInt0, TypeDouble, TypeDouble0]]

shortNamed :: Tokenizer ()
shortNamed = do
    many isAlphaNum
    l <- lexeme
    case lookup l reservedWords of
        Nothing -> emitl NameT
        Just t -> emit t
