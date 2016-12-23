import System.Console.Readline(addHistory, readline)

import Model.Expression(evaluate, toFormula, toString)
import Model.Lexer(tokenize)
import Model.Parser(parse)

main :: IO ()
main = do
    putStrLn "Introduce expressions, exit or Ctrl-D to end."
    loop

loop :: IO()
loop = do
    i <- readline "% "
    case i of
        Nothing -> return ()
	Just "exit" -> return ()
        Just formula -> do
            addHistory formula
            let expression = parse formula
            putStrLn $ "Tokens: " ++ show (tokenize formula)
            putStrLn $ "Expresion: " ++ show expression
            putStrLn $ "As formula: " ++ toFormula expression
            putStrLn $ "Evaluated: " ++ toString (evaluate [] expression)
            loop
