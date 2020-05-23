{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Model.RowStore.ListatabFile (
   -- *Functions
   fromListatab,
   toListatab,
   -- *Reexported
   module Model.RowStore.ListatabInfo
) where

import Control.Exception (displayException, IOException, try, throwIO)
import qualified Data.ByteString.Lazy as BS
import Data.Aeson.Encode.Pretty(encodePretty)
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
import Model.RowStore.Base
import Model.RowStore.ListatabInfo

-- |Reads a listatab file. Returns (possibly) the list of names and the rows.
fromListatab :: ListatabInfo -> FilePath -> IO (Maybe [Text], DataSource)
fromListatab ltInfo fp = do
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
toListatab :: ListatabInfo -> FilePath -> Maybe FilePath -> RowStore -> IO ()
toListatab info fp mconf model = do
    mh <- try (openFile fp WriteMode)
    case mh of
        Right h -> do
                     case names model of
                        Nothing -> return ()
                        Just ns -> case ltHeaderType info of
                                     NoHeader -> return ()
                                     FirstLine -> T.hPutStrLn h $ T.intercalate (T.singleton $ ltOutputSeparator info) ns
                                     Comment -> T.hPutStrLn h $ T.concat ["#<", T.intercalate "><" ns, ">"]
                     mapM_ (writeRow (ltOutputSeparator info) h) $ rows model
                     hClose h
        Left e -> exception e
    case mconf of
        Nothing -> return ()
        Just conf -> do
            r <- try (BS.writeFile conf . encodePretty $ getConf model)
            case r of
                Right () -> return ()
                Left e -> exception e

writeRow :: Char -> Handle -> Row -> IO ()
writeRow sep h = hPutStrLn h . intercalate [sep] . map (encodeString sep . T.unpack . toString)

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
