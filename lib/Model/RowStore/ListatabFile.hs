{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Model.RowStore.ListatabFile (
   -- *Functions
   readListatab,
   writeListatab,
   -- *Reexported
   module Model.RowStore.ListatabInfo
) where

import Control.Exception (try, throwIO)
import Control.Monad(unless)
import Data.List(intercalate)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text(Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Void(Void)
import System.IO (Handle, hClose, hPutStrLn, openFile, IOMode(WriteMode), stdout)
import Text.Megaparsec (many, sepBy, endBy, between, noneOf, optional, parse, Parsec, errorBundlePretty, (<|>))
import Text.Megaparsec.Char(char)
import Text.Megaparsec qualified as TM

import HRowsException
import Model.Field
import Model.Row
import Model.RowStore.ListatabInfo

-- |Reads a listatab file. If the `FilePath` is None, reads from the
-- standard input.
-- Returns (possibly) the list of names and the rows.
readListatab :: ListatabInfo -> Maybe FilePath -> IO (Maybe [Text], DataSource)
readListatab ltInfo mfp = do
  text <- maybe T.getContents readText mfp
  let fp = fromMaybe "stdin" mfp
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
  let sep = ltSeparator ltInfo
      inNameChar = TM.try (char '\\' >> (char '>' <|> char '\\'))
                   <|> noneOf (">" :: String)
  h <- case ltHeaderType ltInfo of
          NoHeader -> return Nothing
          FirstLine -> Just <$> (sepBy (stringParser sep) (char sep) <* char '\n')
          Comment -> optional $
                         between (char '#') (char '\n')
                                   ( many ( T.pack <$> (char '<'
                                                        *> many inNameChar
                                                        <* char '>')
                                          )
                                   )
  rs <- flip endBy (char '\n') $
          flip sepBy (char sep)
            (toField <$> stringParser sep)
  return (h, rs)

stringParser :: Char -> Parser Text
stringParser sep = T.pack <$> ((char '"' *> many inStringChar <* char '"')
                               <|> many (noneOf [sep, '\n']))
    where inStringChar = TM.try (char '\\' >> ( char '\\'
                                       <|> (char 'n' >> return '\n')
                                       <|> (char 't' >> return '\t')
                                       <|> (char '"' >> return '"')
                                       <|> noneOf ("\n\"" ::String)))
                         <|> noneOf ("\n\"" :: String)

-- |Writes a `DataSource` to a listatab file. If the `FilePath` is `Nothing`
-- writes to stdout.
writeListatab :: ListatabInfo -> Maybe FilePath -> Maybe [Text] -> DataSource -> IO ()
writeListatab info mfp mNames ds = do
    let sep = ltSeparator info
    mh <- case mfp of
            Just fp -> try (openFile fp WriteMode)
            Nothing -> return $ Right stdout
    case mh of
        Right h -> do
                     case mNames of
                        Nothing -> return ()
                        Just ns -> case ltHeaderType info of
                                     NoHeader -> return ()
                                     FirstLine -> writeSeparated sep h ns
                                     Comment -> T.hPutStrLn h $ T.concat ["#<", T.intercalate "><" (map encodeName ns), ">"]
                     mapM_ (writeRow sep h) ds
                     unless (isNothing mfp) $ hClose h
        Left e -> exception e

encodeName :: Text -> Text
encodeName = T.replace ">" "\\>" . T.replace "\\" "\\\\"

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
