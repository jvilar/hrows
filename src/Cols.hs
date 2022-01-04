{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImportQualifiedPost #-}

import Control.Exception qualified as E
import Control.Monad(liftM, unless, when)
import Control.Lens(makeLenses, over, set, (^.))
import Data.Default(Default(def))
import Data.List(elemIndex)
import Data.Maybe(fromJust, isJust)
import Data.Text qualified as T
import System.Directory(doesFileExist)
import System.Environment(getArgs, getProgName)
import System.Exit(exitFailure, exitSuccess)
import System.IO(hPutStrLn, stderr, stdout)

import Text.Parsec(alphaNum, between, char, digit, letter, many, many1, noneOf, oneOf, parse, sepBy1, try, (<|>))


import System.Console.JMVOptions

import HRowsException
import Model.DefaultFileNames
import Model.Row
import Model.RowStore hiding (Field)
import Model.SourceInfo


data Field = Numeric Int | Literal String | Range Int Int

data Options = Options { _help :: Bool
                       , _iOptions :: ListatabInfo
                       , _oOptions :: ListatabInfo
                       , _cols :: [Field]
                       , _inputFileName :: Maybe FilePath
                       , _confFileName :: Maybe FilePath
                       }

makeLenses ''Options

defOpts :: Options
defOpts = Options { _help = False
                  , _iOptions = def
                  , _oOptions = def
                  , _cols = [Numeric 1]
                  , _inputFileName = Nothing
                  , _confFileName = Nothing
                  }

-- Parses a String to a Char representing a separator. Recongizes only
-- strings with one char or with a scape followed by a t.
parseSeparator :: String -> Char
parseSeparator [c] = c
parseSeparator "\\t" = '\t'
parseSeparator s = error $ "Illegal string for separator: " ++ show s

setSeparator l s = over l (\oc -> oc { ltInputSeparator = s })

setHeader l c = over l (\oc -> oc { ltHeaderType = c })

options :: [OptDescr (Options -> Options)]
options = processOptions $ do
               '0' ~: "iNoHeader" ==> NoArg (setHeader iOptions NoHeader . setHeader oOptions NoHeader) ~: "Do not use header in the input."
               'O' ~: "oNoHeader" ==> NoArg (setHeader oOptions NoHeader) ~: "Do not use header in the output. Must be used after -0 if both are present."
               '1' ~: "iHeader1" ==> NoArg (setHeader iOptions FirstLine . setHeader oOptions FirstLine) ~: "Use the first line as header in the input."
               'f' ~: "oHeader1" ==> NoArg (setHeader oOptions FirstLine) ~: "Use the first line as header in the output"
               'c' ~: "cols" ==> ReqArg (set cols . parseCols) "COLS" ~: "Column specification. A list of fields separated by comas. A field can specify a column with an integer or a string between simple or double inverted commas, or it can specify a range with two integers separated by a minus sign."
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
                case a of
                   [] -> return opt
                   [f] -> return $ set inputFileName (Just f) opt
                   [f, c] -> return $ set inputFileName (Just f)
                                    $ set confFileName (Just c) opt
                   _ -> myError "Too many filenames"


toRange :: Int -> Maybe Int -> Field
toRange n Nothing = Numeric n
toRange n (Just n') = Range n n'

parseCols :: String -> [Field]
parseCols s = case parse parser "" s of
                     Left _ -> error "Incorrect column especification."
                     Right fs -> fs
                  where parser = flip sepBy1 (char ',') $
                                   try ( Literal <$> ((:) <$> letter <*> many alphaNum)
                                       ) <|>
                                       ( Literal <$>
                                           between (oneOf "\"'")
                                                   (oneOf "\"'")
                                                   (many $ noneOf "\"'")
                                       ) <|>
                                       ( toRange <$> (read <$> many1 digit)
                                                 <*> ((Just . read) <$> ((char '-') *> many1 digit)
                                                       <|> return Nothing)
                                       )

myError :: String -> IO a
myError m = do
              n <- getProgName
              hPutStrLn stderr $ n ++ " error: " ++ m
              exitFailure

helpMessage :: String
helpMessage = usageInfo header options
              where header = "Usage: cols [Options] [files]"

-- load :: Options -> DataSource
-- load opts | opts ^. inputFileName == Nothing -> 

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
          lsts <- case opts ^. inputFileName of
                     Nothing -> error "No file name"
                     Just fs -> load opts
          {-
          let cs = toIndices (opts ^. cols) lsts
              ncs = map (normalize $ map width lsts) cs
          escribe (opts ^. oOptions) stdout $ process lsts cs
          -}
          print $ size lsts

{-
process :: [DataSource] -> [Int] -> DataSource
process lsts cs = let
                      ncs = map (normalize $ map width lsts) cs
                      rs = [ pick ncs l | l <-  toRows $ map registros lsts ]
                      h = if all (isJust . cabecera) lsts
                          then Just $ zip (pick ncs (map (map fst . fromJust . cabecera) lsts)) [1..]
                          else Nothing
                  in mkListatab h rs

-- |Given a list of pairs index and list index returns the
-- corresponding elements from a list of lists of Strings.
pick :: [(Int, Int)] -> [[String]] -> [String]
pick nks ls = map (\(n, k) -> tk (ls !! k) n) nks
  where tk [] _ = ""
        tk (x:_) 0 = x
        tk (_:xs) n = tk xs (n-1)

-- |Given  a list of `widths` ws and an index `i`, return the
--  index of the list in which i falls and the position within
--  it.
normalize :: [Int] -> Int -> (Int, Int)
normalize = go 0
    where go _ [] _ = error "Field number outside the range"
          go k (w:ws) i | i <= w = (i - 1, k)
                        | otherwise = go (k+1) ws (i-w)

-- |Combine a list of columns of lists into a list of rows of
-- lists. I.e, from [["a", "b", "c"], ["1", "2", "3"]] to [["a", "1"],
-- ["b", "2"], ["c", "3"]]. Gaps are filled with empty lists.  Note
-- that in our listatabs, there is still one more level of listness,
-- ie, the registers are lists of lists of strings and we have a list
-- of those.
toRows :: [[[a]]] -> [[[a]]]
toRows l | all null l = []
         | otherwise = map sHead l : toRows (map sTail l)
             where sHead [] = []
                   sHead (x:_) = x
                   sTail [] = []
                   sTail (_:t) = t

pad :: Int -> [[a]] -> [[a]]
pad n l = l ++ replicate n []


toIndices :: [Field] -> [DataSource] -> [Int]
toIndices cs lsts = let
                       names = do
                                 l <- lsts
                                 case cabecera l of
                                    Nothing -> []
                                    Just h -> pad (width l) (map fst h)
                       pos c = case elemIndex c names of
                                  Nothing -> error $ "Column " ++ c ++ " not found."
                                  Just n -> n + 1
                       change (Literal c) =  [pos c]
                       change (Numeric n) = [n]
                       change (Range n n') = [n..n']
                     in concatMap change cs
-}
