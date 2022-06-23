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
-- Returns (possibly) the information on the header.
readListatab :: ListatabInfo -> Maybe FilePath -> IO (Maybe ListatabHeader, DataSource)
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

analyze :: ListatabInfo -> Parser (Maybe ListatabHeader, DataSource)
analyze ltInfo = do
  let sep = ltSeparator ltInfo
      inNameChar = TM.try (char '\\' >> (char '>' <|> char '\\' <|> char '|'))
                   <|> noneOf ("|>" :: String)
  h <- case ltHeaderType ltInfo of
          NoHeader -> return Nothing
          FirstLine -> Just <$> (sepBy (Left <$> stringParser sep) (char sep) <* char '\n')
          Comment -> optional $
                         between (char '#') (char '\n')
                                   ( many (do
                                              _ <- char '<'
                                              name <- T.pack <$> many inNameChar
                                              mtype <- (Just . read <$> (char '|' *> many inNameChar))
                                                       <|> return Nothing
                                              _ <- char '>'
                                              return $ case mtype of
                                                Nothing -> Left name
                                                Just t -> Right (name, t)
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

hname :: Either Text (Text, FieldType) -> Text
hname (Left n) = n
hname (Right (n, _)) = n

-- |Writes a `DataSource` to a listatab file. If the `FilePath` is `Nothing`
-- writes to stdout.
writeListatab :: ListatabInfo -> Maybe FilePath -> Maybe ListatabHeader -> DataSource -> IO ()
writeListatab info mfp mHeader ds = do
    let sep = ltSeparator info
    mh <- case mfp of
            Just fp -> try (openFile fp WriteMode)
            Nothing -> return $ Right stdout
    case mh of
        Right h -> do
                     case mHeader of
                        Nothing -> return ()
                        Just hdr -> case ltHeaderType info of
                                     NoHeader -> return ()
                                     FirstLine -> writeSeparated sep h $ map hname hdr
                                     Comment -> let
                                                   items = map item hdr
                                                   item (Left n) = encodeName n
                                                   item (Right (n, t)) = encodeName n <> "|" <> T.pack (show t)
                                                in T.hPutStrLn h
                                                     $ T.concat [ "#<"
                                                                , T.intercalate "><" items
                                                                , ">"]
                     mapM_ (writeRow sep h) ds
                     unless (isNothing mfp) $ hClose h
        Left e -> exception e

encodeName :: Text -> Text
encodeName = T.replace ">" "\\>" . T.replace "\\" "\\\\" . T.replace "|" "\\|"

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
