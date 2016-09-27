module ListatabFile (
   -- *Functions
   fromListatab,
   toListatab
) where

import Control.Exception (displayException, IOException, try, throwIO)
import Control.Monad (mapM_)
import Data.List(intercalate)
import System.IO (Handle, hGetContents, hPutStrLn, openFile, IOMode(ReadMode, WriteMode))
import Text.Megaparsec hiding (try)

import HRowsException
import SourceInfo
import Model

-- |Creates a `Model` from a listatab file.
fromListatab :: ListatabInfo -> FilePath -> IO Model
fromListatab info fp = do
  mf <- try (openFile fp ReadMode >>= hGetContents)
  case mf of
      Right f -> case parse (analyze (ltInputSeparator info)) fp f of
          Right m -> return m
          Left e -> throwIO $ HRowsException $
                       "Error reading file " ++ fp ++ ":\n"
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

-- |Writes a model to a listatab file.
toListatab :: ListatabInfo -> FilePath -> Model -> IO ()
toListatab info fp model = do
    mh <- try (openFile fp WriteMode)
    case mh of
        Right h -> do
                     case names model of
                        Nothing -> return ()
                        Just ns -> case ltHeaderType info of
                                   NoHeader -> return ()
                                   FirstLine -> hPutStrLn h $ intercalate [ltOutputSeparator info] ns
                                   Comment -> hPutStrLn h $ "#<" ++ intercalate "><" ns ++ ">"
                     mapM_ (writeRow (ltOutputSeparator info) h) $ rows model
        Left e -> throwIO $ HRowsException $ "Exception: " ++ displayException (e :: IOException)

writeRow :: Char -> Handle -> Row -> IO ()
writeRow sep h = hPutStrLn h . intercalate [sep] . map toString
