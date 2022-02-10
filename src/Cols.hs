{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

import Control.Exception qualified as E
import Control.Monad(unless, when)
import Control.Lens(makeLenses, over, set, (^.), Lens')
import Data.Default(Default(def))
import Data.Maybe(isJust)
import Data.Text qualified as T
import System.Environment(getArgs, getProgName)
import System.Exit(exitFailure, exitSuccess)
import System.IO(hPutStrLn, stderr)
import System.IO.Unsafe(unsafePerformIO)

import System.Console.JMVOptions

import Col
import HRowsException
import Model.DefaultFileNames
import Model.RowStore
import Model.SourceInfo
import Model.SourceInfo (mkPathAndConf)


data Options = Options { _help :: Bool
                       , _iOptions :: ListatabInfo
                       , _oOptions :: ListatabInfo
                       , _cols :: [Col]
                       , _inputFileName :: Maybe FilePath
                       , _confFileName :: Maybe FilePath
                       }

makeLenses ''Options

defOpts :: Options
defOpts = Options { _help = False
                  , _iOptions = def
                  , _oOptions = def
                  , _cols = [AllCols]
                  , _inputFileName = Nothing
                  , _confFileName = Nothing
                  }

-- Parses a String to a Char representing a separator. Recongizes only
-- strings with one char or with a scape followed by a t.
parseSeparator :: String -> Char
parseSeparator [c] = c
parseSeparator "\\t" = '\t'
parseSeparator s = error $ "Illegal string for separator: " ++ show s

setSeparator :: Lens' Options ListatabInfo -> Char -> Options -> Options
setSeparator l s = over l (\oc -> oc { ltInputSeparator = s })

setHeader :: Lens' Options ListatabInfo -> HeaderType -> Options -> Options
setHeader l c = over l (\oc -> oc { ltHeaderType = c })

setCols :: String -> Options -> Options
setCols s = case parseCols (T.pack s) of
                 Left e -> myError $ "Bad column espcification: " ++ T.unpack e
                 Right cs -> set cols cs

options :: [OptDescr (Options -> Options)]
options = processOptions $ do
               '0' ~: "iNoHeader" ==> NoArg (setHeader iOptions NoHeader . setHeader oOptions NoHeader) ~: "Do not use header in the input."
               'O' ~: "oNoHeader" ==> NoArg (setHeader oOptions NoHeader) ~: "Do not use header in the output. Must be used after -0 if both are present."
               '1' ~: "iHeader1" ==> NoArg (setHeader iOptions FirstLine . setHeader oOptions FirstLine) ~: "Use the first line as header in the input."
               'f' ~: "oHeader1" ==> NoArg (setHeader oOptions FirstLine) ~: "Use the first line as header in the output"
               'c' ~: "cols" ==> ReqArg setCols "COLS" ~: "Column specification. A list of expressions separated by commas in the format of the formulas of hrows. Also, a range can be specified by two column names or positions separated by a colon and surrounded by square brackes like [$1:$4] or [Name:Surname]."
               'h' ~: "help" ==> NoArg (set help True) ~: "This help."
               's' ~: "separator" ==> ReqArg (\s -> let c = parseSeparator s in setSeparator iOptions c . setSeparator oOptions c) "CHAR" ~:
                        ("Field separator for the input and output. (Default: " ++ show (ltInputSeparator $ defOpts ^. iOptions) ++ ").")
               'S' ~: "oSeparator" ==> ReqArg (setSeparator oOptions . parseSeparator) "CHAR" ~:
                        "Field separator for the output. (Default: same as -s). Must appear after -s when both are present."

getOptions :: IO Options
getOptions = do
                args <- getArgs
                let (o, a, e) = getOpt Permute options args
                let opt = foldl (flip id) defOpts o
                when (opt ^. help) $ putStrLn helpMessage >> exitSuccess
                unless (null e) $ myError $ concat e ++ helpMessage
                return $ case a of
                   [] -> opt
                   [f] -> set inputFileName (Just f) opt
                   [f, c] -> set inputFileName (Just f) $ set confFileName (Just c) opt
                   _ -> myError "Too many filenames"


myError :: String -> a
myError m = unsafePerformIO $ do
              n <- getProgName
              hPutStrLn stderr $ n ++ " error: " ++ m
              exitFailure

helpMessage :: String
helpMessage = usageInfo header options
              where header = "Usage: cols [Options] [files]"

load :: Options -> IO RowStore
load opts = do
    let Just fn = opts ^. inputFileName
    pc <- mkPathAndConf fn $ opts ^. confFileName
    let sinfo =  mkSourceInfo Nothing pc $ opts ^. iOptions

    r <- E.try $ readRowStore sinfo
    case r of
        Right (rst, _) -> return rst
        Left (HRowsException mess) -> myError $ T.unpack mess


main :: IO ()
main = do
          opts <- getOptions
          rst <- case opts ^. inputFileName of
                     Nothing -> readRowStoreStdin $ opts ^. iOptions
                     Just _ -> load opts
          let rst' = applyCols (opts ^. cols) rst
          writeRowStoreStdout (opts ^. oOptions) rst'

