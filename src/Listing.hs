{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
import Control.Applicative(ZipList(..), Alternative ((<|>)))
import Control.Exception qualified as E
import Control.Monad(unless, when)
import Control.Lens
import Data.Default(def)
import Data.List(intercalate, sortOn)
import Data.Map(Map)
import Data.Map qualified as M
import Data.Maybe(catMaybes, isNothing)
import Data.Text(Text)
import Data.Text qualified as T
import System.Environment(getArgs, getProgName)
import System.Exit(exitFailure, exitSuccess)
import System.IO(hPutStrLn, stderr)
import System.IO.Unsafe(unsafePerformIO)

import System.Console.JMVOptions

import Col
import HRowsException
import Model.RowStore
import Model.SourceInfo

data Format = HTML | LaTeX | Listatab deriving (Show, Read, Enum)

type AnonDic = Map Text Text

data ColIndices = ColIndices { _keyIndex :: Int
                             , _markStart :: Int
                             , _extrasStart :: Int
                             , _globalIndex :: Int
                             , _messageIndex :: Int
                             } deriving Show
makeLenses ''ColIndices

data ListingRow = Message Text | Fields [Text]

data Options = Options { _help :: Bool
                       , _anonymize :: Bool
                       , _anonFile :: Maybe FilePath
                       , _anonKey :: Col
                       , _anonLength :: Int
                       , _format :: Format
                       , _minPass :: Double
                       , _canCompensate :: Double
                       , _sortByGlobal :: Bool

                       , _key :: Col
                       , _marks :: [Col]
                       , _decimals :: Int
                       , _global :: Maybe Col
                       , _globalDecimals :: Int
                       , _message :: Maybe Col
                       , _extraCols :: [Col]

                       , _optionsFile :: Maybe FilePath
                       , _iOptions :: ListatabInfo
                       , _oOptions :: ListatabInfo
                       , _inputFileName :: Maybe FilePath
                       , _confFileName :: Maybe FilePath
                       }

makeLenses ''Options

defOpts :: Options
defOpts = Options { _help = False
                  , _anonymize = False
                  , _anonFile = Nothing
                  , _anonKey = Single (mkPosition 0) Nothing
                  , _anonLength = 5
                  , _format = LaTeX
                  , _minPass = 5
                  , _canCompensate = 4
                  , _sortByGlobal = False

                  , _key = Single (mkPosition 0) Nothing
                  , _marks = []
                  , _decimals = 2
                  , _global = Nothing
                  , _globalDecimals = 1
                  , _message = Nothing
                  , _extraCols = []

                  , _optionsFile = Nothing
                  , _iOptions = def
                  , _oOptions = def
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

setCols :: Lens' Options [Col] -> String -> String -> Options -> Options
setCols l n s = case parseCols (T.pack s) of
                 Left e -> myError $ "Bad column especification in " ++ n ++ ": " ++ T.unpack e
                 Right cs -> set l cs

setSingleCol :: Traversal' Options Col -> String -> String -> Options -> Options
setSingleCol l n s = case parseCols (T.pack s) of
                       Left e -> myError $ "Bad column especification in " ++ n ++ ": " ++ T.unpack e
                       Right [c@(Single _ _)]-> set l c
                       Right _ -> myError $ "For " ++ n ++ " you have to specify exactly one column"

setMaybeCol :: Lens' Options (Maybe Col) -> String -> String -> Options -> Options
setMaybeCol l n s = setSingleCol (l . _Just) n s . set l (Just AllCols)

defValue :: Show a => Lens' Options a -> String
defValue l = "Default: " ++ show (defOpts ^. l) ++ "."

options :: [OptDescr (Options -> Options)]
options = processOptions $ do
               'h' ~: "help" ==> NoArg (set help True) ~: "This help."
               '0' ~: "iNoHeader" ==> NoArg (setHeader iOptions NoHeader . setHeader oOptions NoHeader) ~: "Do not use header in the input."
               'O' ~: "oNoHeader" ==> NoArg (setHeader oOptions NoHeader) ~: "Do not use header in the output. Must be used after -0 if both are present."
               '1' ~: "iHeader1" ==> NoArg (setHeader iOptions FirstLine . setHeader oOptions FirstLine) ~: "Use the first line as header in the input."
               'f' ~: "oHeader1" ==> NoArg (setHeader oOptions FirstLine) ~: "Use the first line as header in the output"
               's' ~: "separator" ==> ReqArg (\s -> let c = parseSeparator s in setSeparator iOptions c . setSeparator oOptions c) "CHAR" ~:
                        ("Field separator for the input and output. (Default: " ++ show (ltInputSeparator $ defOpts ^. iOptions) ++ ").")
               'S' ~: "oSeparator" ==> ReqArg (setSeparator oOptions . parseSeparator) "CHAR" ~:
                        "Field separator for the output. (Default: same as -s). Must appear after -s when both are present."
               'a' ~: "anonymize" ==> NoArg (set anonymize True) ~: "Anonymize the key column"
               'A' ~: "anonFile" ==> ReqArg ( (set anonymize True .)
                                                 . set anonFile . Just) "FILE"
                            ~: "Anonymize the key column using the file as reference (implies -a)"

               'K' ~: "anonKey" ==> ReqArg (setSingleCol anonKey "anonKey") "KEY" ~: "Column with the key in the anonymous file. Default: first column."
               'l' ~: "anonLength" ==> ReqArg (set anonLength . read) "INT" ~: "Length of the anoymous keys. " ++ defValue anonLength
               'k' ~: "key" ==> ReqArg (setSingleCol key "key") "KEY" ~: "Column with the key. Default: first column."
               'm' ~: "marks" ==> ReqArg (setCols marks "marks") "COLS" ~: "Columns with the marks. Default: no columns."
               'g' ~: "global" ==> ReqArg (setMaybeCol global "global") "COL" ~: "Column with the global mark."
               'M' ~: "message" ==> ReqArg (setMaybeCol message "message") "COL" ~: "Column that if not empty overrides the others. Default: no column."
               'x' ~: "extraCols" ==> ReqArg (setCols extraCols "extraCols") "COLS" ~: "Columns with additional information"
               'd' ~: "decimals" ==> ReqArg (set decimals . read) "DECS" ~: "Number of decimal places. " ++ defValue decimals
               'D' ~: "globalDecimals" ==> ReqArg (set globalDecimals . read) "DECS" ~: "Number of decimal places of the global mark. " ++ defValue globalDecimals
               'p' ~: "minPass" ==> ReqArg (set minPass . read) "MARK" ~: "Minimum passing mark. " ++ defValue minPass
               'P' ~: "canCompesate" ==> ReqArg (set minPass . read) "MARK" ~: "Minimum mark that can be compensated. " ++ defValue canCompensate
               'F' ~: "format" ==> ReqArg (set format . read) "FORMAT" ~: "Format of the output, one of " ++ showEnum HTML ++ ". " ++ defValue format
               'G' ~: "sortByGlobal" ==> NoArg (set sortByGlobal True) ~: "Sort using the global column instead of the key"

showEnum :: (Enum a, Show a) => a -> String
showEnum = intercalate ", " . map show . enumFrom

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
              where header = "Usage: listing [Options] [files]\
                             \Creates a listing of marks from a listatab file.\n\
                             \Columns can be specified by formulas using hrows syntax.\n\
                             \Options receiving a list of formulas can list them \
                             \separated\n\
                             \by commas and also as ranges like [$1:$3] or [Q1:Q4]."

load :: Options -> IO RowStore
load opts = do
    let Just fn = opts ^. inputFileName
    pc <- mkPathAndConf fn $ opts ^. confFileName
    let sinfo =  mkSourceInfo Nothing pc $ opts ^. iOptions

    r <- E.try $ readRowStore sinfo
    case r of
        Right (rst, _) -> return rst
        Left (HRowsException mess) -> myError $ T.unpack mess


translate :: Options -> Maybe AnonDic -> RowStore -> (RowStore, ColIndices)
translate opts mdic rst = let
    trKey = getKeyCol opts mdic rst
    trMarks = applyCols (opts ^. marks) rst
    trExtras = applyCols (opts ^. extraCols) rst
    trGlobal = applyCols (catMaybes [opts ^. global]) rst
    trMessage = applyCols (catMaybes [opts ^. message]) rst
    allTr = [trKey, trMarks, trExtras, trGlobal, trMessage]
    allRows = map concat . getZipList . sequenceA $
                ZipList . rows <$> allTr
    allNames = concat <$> traverse names allTr
    inds = ColIndices { _keyIndex = 0
                      , _markStart = nFields trKey
                      , _extrasStart = _markStart inds + nFields trMarks
                      , _globalIndex = _extrasStart inds + nFields trExtras
                      , _messageIndex = _globalIndex inds + nFields trGlobal
                      }

  in case allNames of
        Nothing -> (fromRows (getName rst) allRows, inds)
        Just nms -> (fromRowsNames (getName rst) nms allRows, inds)

getKeyCol :: Options -> Maybe AnonDic -> RowStore -> RowStore
getKeyCol opts mdic rst
    | isNothing mdic = col
    | otherwise = mapCol 0 mkAnon col
    where col = applyCols [opts ^. key] rst
          Just dic = mdic
          mkAnon = toField . (dic M.!) . toString

keys :: Col -> RowStore -> [Text]
keys col rst = rst ^..  colF col . element 0 . to toString

anonymizeDic :: Int -> [Text] -> Map Text Text
anonymizeDic ml ts = let
    s = sortOn T.reverse ts
    d = zipWith3 combine s ("":s) (tail s ++ [""])
    combine r p n = maxBy T.length (discriminate r p) (discriminate r n)
    dots t = T.pack (replicate (ml - T.length t) '.') `T.append` t
  in M.fromList $ zip s (map dots d)

maxBy :: Ord o => (a -> o) -> a -> a -> a
maxBy f x y | f x >= f y = x
            | otherwise  = y

discriminate :: Text -> Text -> Text
discriminate ref other = T.pack . sel []
                       $ T.zip (T.reverse ref) (T.reverse other)
    where sel d [] = d
          sel d ((r, o):xs) | r /= o = r:d
                            | otherwise = sel (r:d) xs

createAnonDic :: Options -> RowStore -> IO (Maybe AnonDic)
createAnonDic opts rst = fmap (anonymizeDic (opts ^. anonLength))
                           <$> sequence (fromFile <|> fromRst)
    where fromFile = do
            f <- opts ^. anonFile
            let sinfo = mkSourceInfo Nothing (PathAndConf f Nothing) def
            return (keys (opts ^. anonKey) . fst <$> readRowStore sinfo)
          fromRst = if opts ^. anonymize
                    then Just . return $ keys (opts ^. key) rst
                    else Nothing



main :: IO ()
main = do
          opts <- getOptions
          rst <- case opts ^. inputFileName of
                     Nothing -> readRowStoreStdin $ opts ^. iOptions
                     Just _ -> load opts
          anonDic <- createAnonDic opts rst
          let (rst', inds) = translate opts anonDic rst
          writeRowStoreStdout (opts ^. oOptions) rst'

