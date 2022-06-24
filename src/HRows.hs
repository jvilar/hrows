module Main where

import Control.Auto.Run(runOnChanM)
import Control.Concurrent(forkIO, threadDelay)
import Control.Concurrent.Chan(Chan, newChan, writeChan, writeList2Chan)
import Control.Lens ((^.), set)
import Control.Monad(forM_, unless, void, when)
import Data.Default(Default(..))
import Data.GI.Gtk.Threading(postGUIASync)
import qualified GI.Gtk as Gtk
import System.Environment(getArgs)
import System.Exit(exitSuccess)

import System.Console.JMVOptions

import GUI.Build
import GUI.Update
import Model.SourceInfo
import Presenter
import Col

type Options = ColOptions

options :: [OptDescr (Options -> Options)]
options = colOptions OnlyInputNoFilterOptions id

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
  let sinfo =  mkSourceInfo Nothing pc $ opts ^. iOptions

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

