{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

import Control.Monad(unless, when)
import Control.Lens(makeLenses, set, (^.))
import Data.Default(Default(def))
import Data.Maybe (isNothing)
import System.Environment(getArgs)
import System.Exit(exitSuccess)

import System.Console.JMVOptions

import Col
import Model.DefaultFileNames (defaultConfFileName)
import Model.RowStore (writeRowStore, writeRowStoreStdout, RowStore)
import Model.SourceInfo (mkSourceInfo, PathAndConf(PathAndConf))

data Options = Options { _colSpec :: ColSpec
                       , _cOptions :: ColOptions
                       , _oFile :: Maybe FilePath
                       , _genConf :: Bool
                       }

makeLenses ''Options

instance Default Options where
    def = Options { _colSpec = AllCols
                  , _cOptions = def
                  , _oFile = def
                  , _genConf = True
                  }

options :: [OptDescr (Options -> Options)]
options = colOptions FullIOOptions cOptions ++
          processOptions ( do
               'c' ~: "cols" ==> ReqArg (setCols colSpec "cols") "COLS" ~: "Column specification. A list of expressions separated by commas in the format of the formulas of hrows. Also, a range can be specified by two column names or positions separated by a colon and surrounded by square brackes like [$1:$4] or [Name:Surname]."
               'o' ~: "output" ==> ReqArg (set oFile . Just) "PATH" ~: "Output file (default stdout)"
               'C' ~: "noConf" ==> NoArg (set genConf False) ~: "Do not generate conf file when using -o"
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


doWrite :: Options -> RowStore -> IO ()
doWrite opts
    | isNothing (opts ^. oFile) = writeRowStoreStdout (opts ^. cOptions . oOptions)
    | otherwise = writeRowStore si []
                  where
                     Just f = opts ^. oFile
                     pc = PathAndConf f (if opts ^. genConf
                                         then Just $ defaultConfFileName f
                                         else Nothing)
                     si = mkSourceInfo Nothing pc (opts ^. cOptions . oOptions)


main :: IO ()
main = do
          opts <- getOptions
          rst0 <- readRowStoreFromOptions $ opts ^. cOptions
          let rst = applyCols (opts ^. colSpec) rst0
          doWrite opts rst
