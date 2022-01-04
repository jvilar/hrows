{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Exception(try)
import Control.Monad(unless, when)
import Control.Lens(makeLenses, Getting, (^.), set)
import Data.Default(Default(..))
import Data.Maybe(isJust)
import qualified Data.Text as T
import System.Directory (doesFileExist)
import System.Environment(getArgs, getProgName)
import System.Exit(exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

import System.Console.JMVOptions

import HRowsException
import Model.DefaultFileNames
import Model.RowStore
import Model.SourceInfo
import TUI

data Options = Options { _help :: Bool
                       , _inputFileName :: Maybe FilePath
                       , _confFileName :: Maybe FilePath
                       , _inputSeparator :: Char
                       }

makeLenses ''Options

instance Default Options where
    def = Options  { _help = False
                   , _inputFileName = Nothing
                   , _confFileName = Nothing
                   , _inputSeparator = ltInputSeparator def
                   }

defValue :: Show a => Getting a Options a -> String
defValue field = "Default: " ++ show (def ^. field) ++ "."

options :: [OptDescr (Options -> Options)]
options = processOptions $ do
              'h' ~: s "help" ==> NoArg (set help True) ~: s "This help."
              's' ~: s "separator" ==> ReqArg (set inputSeparator . head) "SEP" ~: s "Separator for input of listatab files. " ++ defValue inputSeparator
          where s :: String -> String
                s = id

getOptions :: IO Options
getOptions = do
               args <- getArgs
               let (o, a, e) =  getOpt Permute options args
               let opt = foldl (flip id) def o
               when (opt ^. help) $ putStrLn helpMessage >> exitSuccess
               unless (null e) $ myError $ concat e ++ helpMessage
               case a of
                   [] -> myError "No filename given"
                   [f] -> return $ set inputFileName (Just f) opt
                   [f, c] -> return $ set inputFileName (Just f)
                                    $ set confFileName (Just c) opt
                   _ -> myError "Too many filenames"

helpMessage :: String
helpMessage = usageInfo header options
              where header = "Usage: vrows [Options] <file> [<conf>]"

myError :: String -> IO a
myError m = do
              n <- getProgName
              hPutStrLn stderr $ n ++ " error: " ++ m
              exitFailure


main :: IO ()
main = do
  opts <- getOptions
  let Just fn = opts ^. inputFileName
  cnf <- if isJust $ opts ^. confFileName
         then return $ opts ^. confFileName
         else do
                let defFn = defaultConfFileName fn
                exFn <- doesFileExist fn
                exCnf <- doesFileExist defFn
                return $ if exFn == exCnf
                         then Just defFn
                         else Nothing
  let pc = PathAndConf fn cnf
      ltinfo = def { ltInputSeparator = opts ^. inputSeparator }
      sinfo =  mkSourceInfo Nothing pc ltinfo

  r <- try $ readRowStore sinfo
  case r of
      Right (rst, _) -> startTUI rst
      Left (HRowsException mess) -> myError $ T.unpack mess


