{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Exception(try)
import Control.Monad(unless, when)
import Control.Monad.State(execState, gets, modify)
import Control.Lens(makeLenses, Getting, (^.), set, over)
import Data.Default(Default(..))
import qualified Data.Text as T
import System.Environment(getArgs, getProgName)
import System.Exit(exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe(unsafePerformIO)

import System.Console.JMVOptions

import Col
import HRowsException
import Model.Expression.Evaluation
import Model.Expression.Manipulate (addPositions)
import Model.Expression.Parser
import Model.RowStore
import Model.SourceInfo
import TUI

data Options = Options { _cols :: [Col]
                       , _cOptions :: ColOptions
                       }

makeLenses ''Options

instance Default Options where
    def = Options  { _cols = [AllCols]
                   , _cOptions = def
                   }

options :: [OptDescr (Options -> Options)]
options = colOptions OnlyInputOptions cOptions ++
          processOptions (
              'c' ~: "cols" ==> ReqArg (appendCols cols "cols") "COLS" ~: "Column specification. A list of expressions separated by commas in the format of the formulas of hrows. Also, a range can be specified by two column names or positions separated by a colon and surrounded by square brackes like [$1:$4] or [Name:Surname]."
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
  let Just fn = opts ^. cOptions . inputFileName
  pc <- mkPathAndConf fn $ opts ^.  cOptions .confFileName
  let sinfo =  mkSourceInfo Nothing pc $ opts ^. cOptions .iOptions

  r <- try $ readRowStore sinfo
  case r of
      Right (rst, _) -> startTUI $ flip execState rst $ do
                                      case opts ^. cOptions . rFilter of
                                           Nothing -> return ()
                                           Just e -> do
                                                        dss <- gets getDataSources
                                                        ex <- gets $ flip addPositions e
                                                        modify . filterRows $ (> 0) . toInt . (\r -> evaluate r dss ex)

                                      modify $ applyCols (opts ^. cols)
      Left (HRowsException mess) -> myError $ T.unpack mess


