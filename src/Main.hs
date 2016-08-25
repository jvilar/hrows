{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Auto.Run(runOnChan)
import Control.Concurrent(forkIO)
import Control.Concurrent.Chan(newChan)
import Control.Lens (makeLenses, (^.), set)
import Control.Monad(unless, void, when)
import Data.Maybe(fromJust)
import Graphics.UI.Gtk (mainGUI, postGUIAsync)
import System.Environment(getArgs, getProgName)
import System.Exit(exitFailure, exitSuccess)
import System.IO (hPutStrLn, IOMode(ReadMode), openFile, stderr)

import System.Console.JMVOptions

import AppState
import DisplayInfo
import GUI
import ListatabFile
import Model
import Presenter

data Options = Options { _help :: Bool
                       , _inputFileName :: Maybe FilePath
                       , _separator :: Char
                       }

makeLenses ''Options

defaultOptions :: Options
defaultOptions = Options  { _help = False
                          , _inputFileName = Nothing
                          , _separator = '\t'
                          }

def field = "Default: " ++ show (defaultOptions ^. field) ++ "."

options :: [OptDescr (Options -> Options)]
options = processOptions $ do
              'h' ~: s "help" ==> NoArg (set help True) ~: s "This help."
              's' ~: s "separator" ==> ReqArg (set separator . head) "SEP" ~: s "Separator for listatab files. " ++ def separator
          where s :: String -> String
                s = id

getOptions :: IO Options
getOptions = do
               args <- getArgs
               let (o, a, e) =  getOpt Permute options args
               let opt = foldl (flip id) defaultOptions o
               when (opt ^. help) $ putStrLn helpMessage >> exitSuccess
               unless (null e) $ myError $ concat e ++ helpMessage
               case a of
                   [] -> myError "No filename given"
                   [f] -> return $ set inputFileName (Just f) opt
                   _ -> myError "Too many filenames"

helpMessage :: String
helpMessage = usageInfo header options
              where header = "Usage: hrows [Options] <file>"

myError :: String -> IO a
myError m = do
              n <- getProgName
              hPutStrLn stderr $ n ++ " error: " ++ m
              exitFailure


main :: IO ()
main = do
  opts <- getOptions
  model0 <- initialModel opts
  let state0 = mkState model0

  inputChan <- newChan
  control <- makeGUI inputChan
  updateGUI control $ buildDisplay state0
  forkIO $ void $ runOnChan (\info -> do
                                 postGUIAsync (updateGUI control info)
                                 print $ position info
                                 return True
                            )
                  inputChan
                  (presenter state0)
  mainGUI

initialModel :: Options -> IO Model
initialModel opts = do
  let name = fromJust $ opts ^. inputFileName
  f <- openFile name ReadMode
  fromListatab name f (opts ^. separator)
