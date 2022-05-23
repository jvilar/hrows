{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

import Control.Exception qualified as E
import Control.Monad(unless, when)
import Control.Monad.State.Strict(execState, gets, modify)
import Control.Lens(makeLenses, set, (^.))
import Data.Default(Default(def))
import Data.Text qualified as T
import System.Environment(getArgs)
import System.Exit(exitSuccess)

import System.Console.JMVOptions

import Col
import HRowsException
import Model.Expression.Evaluation
import Model.Expression.Manipulate (addPositions)
import Model.RowStore
import Model.SourceInfo


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

load :: Options -> IO RowStore
load opts = do
    let Just fn = opts ^. cOptions . inputFileName
    pc <- mkPathAndConf fn $ opts ^. cOptions . confFileName
    let sinfo =  mkSourceInfo Nothing pc $ opts ^. cOptions . iOptions

    r <- E.try $ readRowStore sinfo
    case r of
        Right (rst, _) -> return rst
        Left (HRowsException mess) -> myError $ T.unpack mess


main :: IO ()
main = do
          opts <- getOptions
          rst0 <- case opts ^. cOptions . inputFileName of
                     Nothing -> readRowStoreStdin $ opts ^. cOptions . iOptions
                     Just _ -> load opts
          let rst = flip execState rst0 $ do
                       case opts ^. cOptions . rFilter of
                            Nothing -> return ()
                            Just e -> do
                                         dss <- gets getDataSources
                                         ex <- gets $ flip addPositions e
                                         modify . filterRows $ (> 0) . toInt . (\r -> evaluate r dss ex)
                       modify $ applyCols (opts ^. cols)
          writeRowStoreStdout (opts ^. cOptions . oOptions) rst

