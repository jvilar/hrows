module Model.ListatabFile (
   -- *Functions
   fromListatab,
   toListatab
) where

import Control.Exception (displayException, IOException, try, throwIO)
import Control.Monad (mapM_)
import qualified Data.ByteString.Lazy as BS
import Data.Aeson(decode)
import Data.Aeson.Encode.Pretty(encodePretty)
import Data.List(intercalate)
import System.IO (Handle, hClose, hPutStrLn, openFile, readFile, IOMode(ReadMode, WriteMode))
import Text.Megaparsec hiding (try)

import HRowsException
import Model
import Model.ModelConf
import Model.SourceInfo

-- |Creates a `Model` from a listatab file.
fromListatab :: ListatabInfo -> FilePath -> Maybe FilePath -> IO Model
fromListatab info fp mconf = do
  conf <- case mconf of
             Nothing -> return Nothing
             Just fp -> do
                 mf <- try $ do
                               l <- BS.readFile fp
                               if BS.length l > 0
                                   then return l
                                   else throwIO $ HRowsException $ "Empty config file: " ++ fp
                 case mf of
                     Right s -> case decode s of
                                    Nothing -> throwIO $ HRowsException $ "Bad config file: " ++ fp
                                    Just c -> return $ Just c
                     Left e -> exception e
  mr <- try $ do
             l <- readFile fp
             if length l > 0
                 then return l
                 else throwIO $ HRowsException $ "Empty rows file: " ++ fp
  rows <- case mr of
              Right r -> return r
              Left e -> exception e
  case parse (analyze (ltInputSeparator info) conf) fp rows of
      Right m -> return m
      Left e -> throwIO $ HRowsException $
                       "Error reading file " ++ fp ++ ":\n"
                        ++ parseErrorPretty e

exception :: IOException -> IO a
exception e = throwIO $ HRowsException $ "Exception: " ++ displayException e


analyze :: Char -> Maybe ModelConf -> Parsec Dec String Model
analyze separator mconf = do
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
  return $ case mconf of
                  Nothing -> case h of
                                 Nothing -> fromRows rs
                                 Just names -> fromRowsNames names rs
                  Just cnf -> fromRowsConf cnf rs

-- |Writes a model to a listatab file.
toListatab :: ListatabInfo -> FilePath -> Maybe FilePath -> Model -> IO ()
toListatab info fp mconf model = do
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
writeRow sep h = hPutStrLn h . intercalate [sep] . map toString
