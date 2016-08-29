module ListatabFile (
   -- *Functions
   fromListatab
) where

import System.IO (hGetContents, openFile, IOMode(ReadMode))
import Text.Megaparsec

import ListatabInfo
import Model

-- |Creates a `Model` from a listatab file.
fromListatab :: ListatabInfo -> IO Model
fromListatab info = do
  handle <- openFile (ltFileName info) ReadMode
  f <- hGetContents handle
  case parse (analyze (ltInputSeparator info)) (ltFileName info) f of
    Right m -> return $ setSourceInfo info m
    Left e -> do
      putStrLn $ "Error reading file: " ++ (ltFileName info)
      putStr $ parseErrorPretty e
      fail ""

analyze :: Char -> Parsec Dec String Model
analyze separator = do
  h <-  optional $
          between (char '#') (char '\n')
                    ( many ( between (char '<')
                                     (char '>')
                                     $ many (noneOf ">")
                           )
                    )
  rs <- flip endBy (char '\n') $
          flip sepBy (char separator)
            (toField <$> many (noneOf (separator : "\n")))
  let model = fromRows rs
  return $ case h of
             Nothing -> model
             Just h' -> setNames h' model
