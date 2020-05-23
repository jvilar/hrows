{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Model.RowStore.ListatabFile (
   -- *Functions
   readListatab,
   writeListatab,
   -- *Reexported
   module Model.RowStore.ListatabInfo
) where

import Control.Exception (try, throwIO)
import Data.List(intercalate)
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void(Void)
import System.IO (Handle, hClose, hPutStrLn, openFile, IOMode(WriteMode))
import Text.Megaparsec (many, sepBy, endBy, between, noneOf, optional, parse, Parsec, errorBundlePretty, (<|>))
import Text.Megaparsec.Char(char)
import qualified Text.Megaparsec as TM

import HRowsException
import Model.Field
import Model.Row
import Model.RowStore.ListatabInfo

-- |Reads a listatab file. Returns (possibly) the list of names and the rows.
readListatab :: ListatabInfo -> FilePath -> IO (Maybe [Text], DataSource)
readListatab ltInfo fp = do
  text <- readText fp
  case parse (analyze ltInfo) fp text of
     Right r -> return r
     Left e -> throwIO $ hRowsException $
                   "Error reading file " ++ fp ++ ":\n"
                    ++ errorBundlePretty e

readText :: FilePath -> IO Text
readText fp = try (T.readFile fp) >>= \case
      Right l -> return l
      Left e -> exception e

type Parser = Parsec Void Text

analyze :: ListatabInfo -> Parser (Maybe [Text], DataSource)
analyze ltInfo = do
  let sep = ltInputSeparator ltInfo
  h <- case ltHeaderType ltInfo of
          NoHeader -> return Nothing
          FirstLine -> Just <$> sepBy (stringParser sep) (char sep)
          Comment -> optional $
                         between (char '#') (char '\n')
                                   ( many ( T.pack <$> between (char '<')
                                                    (char '>')
                                                    (many $ noneOf (">":: String))
                                          )
                                   )
  rs <- flip endBy (char '\n') $
          flip sepBy (char sep)
            (toField <$> stringParser sep)
  return (h, rs)

stringParser :: Char -> Parser Text
stringParser sep = T.pack <$> ((char '"' *> (many inStringChar <* char '"'))
                               <|> many (noneOf [sep, '\n']))
    where inStringChar = TM.try (char '\\' >> ( char '\\'
                                       <|> (char 'n' >> return '\n')
                                       <|> (char 't' >> return '\t')
                                       <|> (char '"' >> return '"')
                                       <|> noneOf ("\n\"" ::String)))
                         <|> noneOf ("\n\"" :: String)

-- |Writes a `RowStore` to a listatab file.
-- |Writes a `DataSource` to a listatab file.
writeListatab :: ListatabInfo -> FilePath -> Maybe [Text] -> DataSource -> IO ()
writeListatab info fp mNames ds = do
    let sep = ltOutputSeparator info
    mh <- try (openFile fp WriteMode)
    case mh of
        Right h -> do
                     case mNames of
                        Nothing -> return ()
                        Just ns -> case ltHeaderType info of
                                     NoHeader -> return ()
                                     FirstLine -> writeSeparated sep h ns
                                     Comment -> T.hPutStrLn h $ T.concat ["#<", T.intercalate "><" ns, ">"]
                     mapM_ (writeRow sep h) ds
                     hClose h
        Left e -> exception e

writeRow :: Char -> Handle -> Row -> IO ()
writeRow sep h = writeSeparated sep h . map toString

writeSeparated :: Char -> Handle -> [Text] -> IO ()
writeSeparated sep h = hPutStrLn h . intercalate [sep] . map (encodeString sep . T.unpack)

encodeString :: Char -> String -> String
encodeString sep s = case preprocess s of
                         (True, s') -> '"':(s' ++ "\"")
                         (False, _) -> s
    where preprocess [] = (False, [])
          preprocess (x:xs) | x == sep = (True, x: pxs)
                            | x == '\\' = (True, '\\':'\\': pxs)
                            | x == '\n' = (True, '\\':'n': pxs)
                            | x == '\t' = (True, '\\':'t': pxs)
                            | x == '"' =  (True, '\\':'"': pxs)
                            | otherwise = (bxs, x:pxs)
                            where (bxs, pxs) = preprocess xs
