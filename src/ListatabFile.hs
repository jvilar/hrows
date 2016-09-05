module ListatabFile (
   -- *Functions
   fromListatab
) where

import Control.Exception (displayException, IOException, try, throwIO)
import System.IO (hGetContents, openFile, IOMode(ReadMode))
import Text.Megaparsec hiding (try)

import HRowsException
import ListatabInfo
import Model

-- |Creates a `Model` from a listatab file.
fromListatab :: ListatabInfo -> IO Model
fromListatab info = do
  mf <- try (openFile (ltFileName info) ReadMode >>= hGetContents)
  case mf of
      Right f -> case parse (analyze (ltInputSeparator info)) (ltFileName info) f of
          Right m -> return $ setSourceInfo info m
          Left e -> throwIO $ HRowsException $
                       "Error reading file " ++ (ltFileName info) ++ ":\n"
                        ++ parseErrorPretty e
      Left e -> throwIO $ HRowsException $ "Exception: " ++ displayException (e :: IOException)

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
