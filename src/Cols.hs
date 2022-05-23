{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

import Control.Monad(unless, when)
import Control.Lens(makeLenses, set, (^.))
import Data.Default(Default(def))
import System.Environment(getArgs)
import System.Exit(exitSuccess)

import System.Console.JMVOptions

import Col
import Model.RowStore


data Options = Options { _cols :: [Col]
                       , _cOptions :: ColOptions
                       }

makeLenses ''Options

instance Default Options where
    def = Options { _cols = [AllCols]
                  , _cOptions = def
                  }

options :: [OptDescr (Options -> Options)]
options = colOptions FullIOOptions cOptions ++
          processOptions (
               'c' ~: "cols" ==> ReqArg (appendCols cols "cols") "COLS" ~: "Column specification. A list of expressions separated by commas in the format of the formulas of hrows. Also, a range can be specified by two column names or positions separated by a colon and surrounded by square brackes like [$1:$4] or [Name:Surname]."
                        )

getOptions :: IO Options
getOptions = do
                args <- getArgs
                let (o, a, e) = getOpt Permute options args
                let opt = foldl (flip id) def o
                when (opt ^. cOptions . help) $ putStrLn helpMessage >> exitSuccess
                unless (null e) $ myError $ concat e ++ helpMessage
                return $ case a of
                   [] -> opt
                   [f] -> set (cOptions . inputFileName) (Just f) opt
                   [f, c] -> set (cOptions . inputFileName) (Just f) 
                           $ set (cOptions . confFileName) (Just c) opt
                   _ -> myError "Too many filenames"


helpMessage :: String
helpMessage = usageInfo header options
              where header = "Usage: cols [Options] [files]"


main :: IO ()
main = do
          opts <- getOptions
          rst0 <- readRowStoreFromOptions $ opts ^. cOptions
          let rst = applyCols (opts ^. cols) rst0
          writeRowStoreStdout (opts ^. cOptions . oOptions) rst

