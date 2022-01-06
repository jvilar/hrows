{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Exception(try)
import Control.Monad(unless, when)
import Control.Lens(makeLenses, Getting, (^.), set, over)
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
                       , _inputOptions :: ListatabInfo
                       }

makeLenses ''Options

instance Default Options where
    def = Options  { _help = False
                   , _inputFileName = Nothing
                   , _confFileName = Nothing
                   , _inputOptions = def
                   }

defValue :: Show a => Getting a Options a -> String
defValue field = "Default: " ++ show (def ^. field) ++ "."

parseSeparator :: String -> Char
parseSeparator [c] = c
parseSeparator "\\t" = '\t'
parseSeparator s = error $ "Illegal string for separator: " ++ show s

setSeparator :: Char -> Options -> Options
setSeparator c = over inputOptions (\oc -> oc { ltInputSeparator = c })

setHeader :: HeaderType -> Options -> Options
setHeader h = over inputOptions (\oc -> oc { ltHeaderType = h })

options :: [OptDescr (Options -> Options)]
options = processOptions $ do
              '0' ~: "NoHeader" ==> NoArg (setHeader NoHeader) ~: "Do not use header in the input."
              '1' ~: "Header1" ==> NoArg (setHeader FirstLine) ~: "Use the first line as header in the input."
              'h' ~: s "help" ==> NoArg (set help True) ~: s "This help."
              's' ~: s "separator" ==> ReqArg (setSeparator . parseSeparator) "SEP" ~: s "Separator for input of listatab files. " ++ defValue inputOptions
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
      sinfo =  mkSourceInfo Nothing pc $ opts ^. inputOptions

  r <- try $ readRowStore sinfo
  case r of
      Right (rst, _) -> startTUI rst
      Left (HRowsException mess) -> myError $ T.unpack mess


