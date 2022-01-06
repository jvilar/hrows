{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

import Control.Exception qualified as E
import Control.Monad(unless, when)
import Control.Lens(makeLenses, over, set, (^.), Traversal', (%~), Lens', traversed, (&))
import Data.Default(Default(def))
import Data.Maybe(isJust)
import Data.Text qualified as T
import System.Directory(doesFileExist)
import System.Environment(getArgs, getProgName)
import System.Exit(exitFailure, exitSuccess)
import System.IO(hPutStrLn, stderr)
import System.IO.Unsafe(unsafePerformIO)

import System.Console.JMVOptions

import HRowsException
import Model.DefaultFileNames
import Model.Row
import Model.RowStore
import Model.SourceInfo
import Model.Expression.Evaluation
import Model.Expression.Manipulate
import Model.Expression.Lexer (Token(EOFT, CommaT, ColonT, ErrorT))
import Model.Expression.Parser
import Model.Expression.RecursionSchemas


data Col = Single Expression | Range Expression Expression deriving Show

expressionT :: Traversal' Col Expression
expressionT f (Single e) = Single <$> f e
expressionT f (Range e1 e2) = Range <$> f e1 <*> f e2

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
                  , _cols = [Range (mkPosition 0) (mkPosition 0)]
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

options :: [OptDescr (Options -> Options)]
options = processOptions $ do
               '0' ~: "iNoHeader" ==> NoArg (setHeader iOptions NoHeader . setHeader oOptions NoHeader) ~: "Do not use header in the input."
               'O' ~: "oNoHeader" ==> NoArg (setHeader oOptions NoHeader) ~: "Do not use header in the output. Must be used after -0 if both are present."
               '1' ~: "iHeader1" ==> NoArg (setHeader iOptions FirstLine . setHeader oOptions FirstLine) ~: "Use the first line as header in the input."
               'f' ~: "oHeader1" ==> NoArg (setHeader oOptions FirstLine) ~: "Use the first line as header in the output"
               'c' ~: "cols" ==> ReqArg (set cols . parseCols) "COLS" ~: "Column specification. A list of expressions separated by commas in the format of the formulas of hrows. Also, a range can be specified by two column names or positions separated by a colon and surrounded by square brackes like [$1:$4] or [Name:Surname]."
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


parseCols :: String -> [Col]
parseCols s = case parse colParser (T.pack s) of
                 Left err -> myError $ "Bad cols specification: " ++ T.unpack err
                 Right xs -> xs

colParser :: Parser [Col]
colParser = do
    t <- current
    c <- case t of
             ErrorT "[" -> rangeParser
             _ -> Single <$> expression
    check EOFT >>= \case
        True -> return [c]
        False -> expect CommaT "a comma" >> (c:) <$> colParser

rangeParser :: Parser Col
rangeParser = do
    advance
    e1 <- expression
    expect ColonT "a colon"
    e2 <- expression
    expect (ErrorT "]")  "a closing square bracket"
    return $ Range e1 e2

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
    cnf <- if isJust $ opts ^. confFileName
           then return $ opts ^. confFileName
           else do
                   let defFn = defaultConfFileName fn
                   exFn <- doesFileExist fn
                   exCnf <- doesFileExist defFn
                   return $ if exFn == exCnf
                            then Just defFn
                            else Nothing
    let pc = PathAndConf fn cnf
        sinfo =  mkSourceInfo Nothing pc $ opts ^. iOptions

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
          let rst' = process opts rst
          writeRowStoreStdout (opts ^. oOptions) rst'

process :: Options -> RowStore -> RowStore
process opts rst = case names rst of
                      Nothing -> fromRows "cols" . map (processRow cs) $ rows rst
                      Just _ -> fromRowsNames "cols" ns . map (processRow cs) $ rows rst
    where cs = opts ^. cols & traversed . expressionT %~ addPositions rst
          ns = concatMap toName cs
          toName (Single e) = [toFormula e]
          toName (Range e1 e2) = case names rst of
                   Nothing -> [ T.pack ("Column " ++ show i) | i <- [p1 + 1..p2 + 1] ]
                   Just xs -> slice p1 p2 xs
               where p1 = pos e1
                     p2 = pos e2

processRow :: [Col] -> Row -> Row
processRow cs r = concatMap f cs
    where f (Single e) = [evaluate r [] e]
          f (Range e1 e2) = slice (pos e1) (pos e2) r

slice :: Int -> Int -> [a] -> [a]
slice p1 p2 = take (p2 - p1 + 1) . drop p1

pos :: Expression -> Int
pos (In (Position n)) = n
pos (In (NamedPosition _ (Just n))) = n
pos e = myError $ "Expression "
                ++ T.unpack (toFormula e)
                ++ " does not represent a position"
