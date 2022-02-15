{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Auto.Run(runOnChanM)
import Control.Concurrent(forkIO, threadDelay)
import Control.Concurrent.Chan(Chan, newChan, writeChan, writeList2Chan)
import Control.Lens (makeLenses, (^.), set, Getting)
import Control.Monad(forM_, unless, void, when)
import Data.Default(Default(..))
import Data.GI.Gtk.Threading(postGUIASync)
import qualified GI.Gtk as Gtk
import System.Environment(getArgs, getProgName)
import System.Exit(exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

import System.Console.JMVOptions

import GUI.Build
import GUI.Update
import Model.DefaultFileNames
import Model.SourceInfo
import Presenter

data Options = Options { _help :: Bool
                       , _inputFileName :: Maybe FilePath
                       , _confFileName :: Maybe FilePath
                       , _inputSeparator :: Char
                       , _outputSeparator :: Char
                       }

makeLenses ''Options

instance Default Options where
    def = Options  { _help = False
                   , _inputFileName = Nothing
                   , _confFileName = Nothing
                   , _inputSeparator = ltSeparator def
                   , _outputSeparator = ltSeparator def
                   }

defValue :: Show a => Getting a Options a -> String
defValue field = "Default: " ++ show (def ^. field) ++ "."

options :: [OptDescr (Options -> Options)]
options = processOptions $ do
              'h' ~: s "help" ==> NoArg (set help True) ~: s "This help."
              's' ~: s "separator" ==> ReqArg ((\c -> set inputSeparator c . set outputSeparator c) . head) "SEP" ~: s "Separator for input of listatab files. " ++ defValue inputSeparator
              -- TODO 'S' ~: s "oSeparator" ==> ReqArg (set outputSeparator . head) "SEP" ~: s "Separator for output listatab files. Default: use the one passed to separator."
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
                   [f, c] -> return $ set inputFileName (Just f) $ set confFileName (Just c) opt
                   _ -> myError "Too many filenames"

helpMessage :: String
helpMessage = usageInfo header options
              where header = "Usage: hrows [Options] <file> [<conf>]"

myError :: String -> IO a
myError m = do
              n <- getProgName
              hPutStrLn stderr $ n ++ " error: " ++ m
              exitFailure

-- |Thread to write a backup every minute
backupLoop :: Chan Input -> IO ()
backupLoop inputChan = do
    threadDelay $ 60 * 1000000
    writeChan inputChan $ toInput WriteBackup
    backupLoop inputChan

main :: IO ()
main = do
  opts <- getOptions
  let Just fn = opts ^. inputFileName
  pc <- mkPathAndConf fn $ opts ^. confFileName
  let ltinfo = def { ltSeparator = opts ^. inputSeparator }
      sinfo =  mkSourceInfo Nothing pc ltinfo

  inputChan <- newChan
  control <- makeGUI inputChan
  _ <- forkIO $ void $ runOnChanM id
                            (updateScreen control)
                            inputChan
                            (presenter sinfo)
  _ <- forkIO $ backupLoop inputChan
  writeList2Chan inputChan [ toInput MoveBegin
                           , toInput $ SetMainSource sinfo
                           , toInput LoadFile]
  Gtk.main

updateScreen :: GUIControl -> [GUICommand] -> IO Bool
updateScreen control commands = do
  forM_ commands $ \command -> postGUIASync (updateGUI command control)
  return True

