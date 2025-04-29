{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad(unless, when)
import Control.Lens(makeLenses, (^.), set)
import Data.Default(Default(..))
import System.Environment(getArgs)
import System.Exit(exitSuccess)

import System.Console.JMVOptions

import Col
import TUI

data Options = Options { _colSpec :: ColSpec
                       , _cOptions :: ColOptions
                       }

makeLenses ''Options

instance Default Options where
    def = Options  { _colSpec = AllCols
                   , _cOptions = def
                   }

options :: [OptDescr (Options -> Options)]
options = colOptions OnlyInputOptions cOptions ++
          processOptions (
              'c' ~: "cols" ==> ReqArg (setCols colSpec "vrows") "COLS" ~: "Column specification. A list of expressions separated by commas in the format of the formulas of hrows. Also, a range can be specified by two column names or positions separated by a colon and surrounded by square brackes like [$1:$4] or [Name:Surname]."
              )

getOptions :: IO Options
getOptions = do
               args <- getArgs
               let (o, a, e) =  getOpt Permute options args
               let opt = foldl (flip id) def o
               when (opt ^. cOptions . help) $ putStrLn helpMessage >> exitSuccess
               unless (null e) $ myError $ concat e ++ helpMessage
               case a of
                   [] -> myError "No filename given"
                   [f] -> return $ set (cOptions . inputFileName) (Just f) opt
                   [f, c] -> return $ set (cOptions . inputFileName) (Just f)
                                    $ set (cOptions . confFileName) (Just c) opt
                   _ -> myError "Too many filenames"

helpMessage :: String
helpMessage = usageInfo header options
              where header = "Usage: vrows [Options] <file> [<conf>]"


main :: IO ()
main = do
  opts <- getOptions
  rst0 <- readRowStoreFromOptions $ opts ^. cOptions
  let rst = applyCols (opts ^. colSpec) rst0
  startTUI rst
