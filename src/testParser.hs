{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import System.Console.Haskeline(outputStrLn, runInputT, InputT, defaultSettings, getInputLine)

import Model.Expression(toFormula, toString)
import Model.Expression.Evaluation(evaluate)
import Model.Expression.Lexer(tokenize)
import Model.Expression.Parser(parseExpression)

main :: IO ()
main = do
    putStrLn "Introduce expressions, exit or Ctrl-D to end."
    runInputT defaultSettings loop

loop :: InputT IO ()
loop = do
    l <- getInputLine "% "
    case T.pack <$> l of
        Nothing -> return ()
        Just "exit" -> return ()
        Just formula -> do
            let expression = parseExpression formula
                ev = evaluate [] [] expression
            outputStrLn $ "Tokens: " ++ show (tokenize formula)
            outputStrLn $ "Expresion: " ++ show expression
            outputStrLn $ "As formula: " ++ T.unpack (toFormula expression)
            outputStrLn $ "Evaluated as string: " ++ T.unpack (toString ev)
            outputStrLn $ "Evaluated: " ++ show ev
            loop
