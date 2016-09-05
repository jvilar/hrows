{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Auto.Run(runOnChanM)
import Control.Concurrent(forkIO)
import Control.Concurrent.Chan(newChan, writeChan)
import Control.Lens (makeLenses, (^.), set, Getting)
import Control.Monad(unless, void, when)
import Graphics.UI.Gtk (mainGUI, postGUISync)
import System.Environment(getArgs, getProgName)
import System.Exit(exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

import System.Console.JMVOptions

import AppState
import DisplayInfo
import GUI
import Model
import Presenter

data Options = Options { _help :: Bool
                       , _inputFileName :: Maybe FilePath
                       , _inputSeparator :: Char
                       , _outputSeparator :: Char
                       }

makeLenses ''Options

defaultOptions :: Options
defaultOptions = Options  { _help = False
                          , _inputFileName = Nothing
                          , _inputSeparator = '\t'
                          , _outputSeparator = '\t'
                          }

def :: Show a => Getting a Options a -> String
def field = "Default: " ++ show (defaultOptions ^. field) ++ "."

options :: [OptDescr (Options -> Options)]
options = processOptions $ do
              'h' ~: s "help" ==> NoArg (set help True) ~: s "This help."
              's' ~: s "separator" ==> ReqArg ((\c -> set inputSeparator c . set outputSeparator c) . head) "SEP" ~: s "Separator for input of listatab files. " ++ def inputSeparator
              'S' ~: s "oSeparator" ==> ReqArg (set outputSeparator . head) "SEP" ~: s "Separator for output listatab files. Default: use the one passed to separator."
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
  let Just fileName = opts ^. inputFileName
      info = ListatabInfo fileName
                          (opts ^. inputSeparator)
                          (opts ^. outputSeparator)
      model0 = empty
      state0 = mkState model0

  inputChan <- newChan
  control <- makeGUI inputChan
  forkIO $ void $ runOnChanM id
                            (updateScreen control)
                            inputChan
                            (presenter state0)
  writeChan inputChan $ InputFile (LoadFile $ toSourceInfo info)
  mainGUI

updateScreen :: GUIControl -> DisplayInfo -> IO Bool
updateScreen control info = do
  postGUISync (updateGUI control info)
  return True

