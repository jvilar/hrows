module ListatabFile (
   -- *Functions
   fromListatab
) where

import System.IO (Handle, hGetContents)
import Text.Megaparsec

import Model

-- |Creates a `Model` from a listatab file.
fromListatab :: FilePath -> Handle -> Char -> IO Model
fromListatab name handle separator = do
  f <- hGetContents handle
  case parse (analyze separator) name f of
    Right lst -> return lst
    Left e -> do
      putStrLn $ "Error reading file: " ++ name
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
