{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

import Control.Applicative(ZipList(..), Alternative ((<|>)))
import Control.Arrow((&&&))
import Control.Exception qualified as E
import Control.Monad(unless, when, forM_)
import Control.Lens
import Data.Default(def)
import Data.List(intercalate, sortOn)
import Data.Map(Map)
import Data.Map qualified as M
import Data.Maybe(catMaybes, isNothing)
import Data.Text(Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Environment(getArgs, getProgName)
import System.Exit(exitFailure, exitSuccess)
import System.IO(hPutStrLn, stderr, IOMode (ReadMode), openFile)
import System.IO.Unsafe(unsafePerformIO)

import System.Console.JMVOptions

import Col
import HRowsException
import Model.Row
import Model.RowStore
import Model.SourceInfo
import Numeric (showFFloat)

data Format = HTML | LaTeX | Listatab deriving (Show, Read, Enum, Eq)

type AnonDic = Map Text Text

data ColIndices = ColIndices { _keyIndex :: Int
                             , _markStart :: Int
                             , _globalIndex :: Int
                             , _extrasStart :: Int
                             , _messageIndex :: Int
                             } deriving Show
makeLenses ''ColIndices

totalCols :: Getter ColIndices Int
totalCols = messageIndex

markEnd :: Getter ColIndices Int
markEnd = globalIndex

markInterval :: Getter ColIndices (Int, Int)
markInterval = to $ view markStart &&& view markEnd

extrasEnd :: Getter ColIndices Int
extrasEnd = messageIndex

extrasInterval :: Getter ColIndices (Int, Int)
extrasInterval = to $ view extrasStart &&& view extrasEnd

globalEnd :: Getter ColIndices Int
globalEnd = extrasStart

globalInterval :: Getter ColIndices (Int, Int)
globalInterval = to $ view globalIndex &&& view globalEnd

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
parseSeparator s = myError $ "Illegal string for separator: " ++ show s

setSeparator :: Lens' Options ListatabInfo -> Char -> Options -> Options
setSeparator l s = over l (\oc -> oc { ltSeparator = s })

setHeader :: Lens' Options ListatabInfo -> HeaderType -> Options -> Options
setHeader l c = over l (\oc -> oc { ltHeaderType = c })

appendCols :: Lens' Options [Col] -> String -> String -> Options -> Options
appendCols l n s = case parseCols (T.pack s) of
                 Left e -> myError $ "Bad column especification in " ++ n ++ ": " ++ T.unpack e
                 Right cs -> over l (++cs)

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
               'o' ~: "optionsFile" ==> ReqArg (set optionsFile . Just) "FILE" ~: "Read the options from a file. Each line in the file has a long option. If there is a parameter, it is written after a colon."
               '0' ~: "iNoHeader" ==> NoArg (setHeader iOptions NoHeader . setHeader oOptions NoHeader) ~: "Do not use header in the input."
               'O' ~: "oNoHeader" ==> NoArg (setHeader oOptions NoHeader) ~: "Do not use header in the output. Must be used after -0 if both are present."
               '1' ~: "iHeader1" ==> NoArg (setHeader iOptions FirstLine . setHeader oOptions FirstLine) ~: "Use the first line as header in the input."
               'H' ~: "oHeader1" ==> NoArg (setHeader oOptions FirstLine) ~: "Use the first line as header in the output"
               's' ~: "separator" ==> ReqArg (\s -> let c = parseSeparator s in setSeparator iOptions c . setSeparator oOptions c) "CHAR" ~:
                        ("Field separator for the input and output. (Default: " ++ show (ltSeparator $ defOpts ^. iOptions) ++ ").")
               'S' ~: "oSeparator" ==> ReqArg (setSeparator oOptions . parseSeparator) "CHAR" ~:
                        "Field separator for the output. (Default: same as -s). Must appear after -s when both are present."
               'a' ~: "anonymize" ==> NoArg (set anonymize True) ~: "Anonymize the key column"
               'A' ~: "anonFile" ==> ReqArg ( (set anonymize True .)
                                                 . set anonFile . Just) "FILE"
                            ~: "Anonymize the key column using the file as reference (implies -a)"

               'K' ~: "anonKey" ==> ReqArg (setSingleCol anonKey "anonKey") "KEY" ~: "Column with the key in the anonymous file. Default: first column."
               'l' ~: "anonLength" ==> ReqArg (set anonLength . read) "INT" ~: "Length of the anoymous keys. " ++ defValue anonLength
               'k' ~: "key" ==> ReqArg (setSingleCol key "key") "KEY" ~: "Column with the key. Default: first column."
               'm' ~: "marks" ==> ReqArg (appendCols marks "marks") "COLS" ~: "Columns with the marks. May appear more than once. Default: no columns."
               'g' ~: "global" ==> ReqArg (setMaybeCol global "global") "COL" ~: "Column with the global mark."
               'M' ~: "message" ==> ReqArg (setMaybeCol message "message") "COL" ~: "Column that if not empty overrides the others. Default: no column."
               'x' ~: "extraCols" ==> ReqArg (appendCols extraCols "extraCols") "COLS" ~: "Columns with additional information. May apper more than once. Default: no extras."
               'd' ~: "decimals" ==> ReqArg (set decimals . read) "DECS" ~: "Number of decimal places. " ++ defValue decimals
               'D' ~: "globalDecimals" ==> ReqArg (set globalDecimals . read) "DECS" ~: "Number of decimal places of the global mark. " ++ defValue globalDecimals
               'p' ~: "minPass" ==> ReqArg (set minPass . read) "MARK" ~: "Minimum passing mark. " ++ defValue minPass
               'P' ~: "canCompensate" ==> ReqArg (set canCompensate . read) "MARK" ~: "Minimum mark that can be compensated. " ++ defValue canCompensate
               'F' ~: "format" ==> ReqArg (set format . read) "FORMAT" ~: "Format of the output, one of " ++ showEnum HTML ++ ". " ++ defValue format
               'G' ~: "sortByGlobal" ==> NoArg (set sortByGlobal True) ~: "Sort using the global column instead of the key"

showEnum :: (Enum a, Show a) => a -> String
showEnum = intercalate ", " . map show . enumFrom

getOptions :: IO Options
getOptions = do
                args <- getArgs
                let (o, a, e) = getOpt Permute options args
                let opt1 = foldl (&) defOpts o
                when (opt1 ^. help) $ putStrLn helpMessage >> exitSuccess
                unless (null e) $ myError $ concat e ++ "\n" ++ helpMessage
                optf <- traverse  (\f -> openFile f ReadMode >>= optionsFromHandle options)
                                 (opt1 ^. optionsFile)
                let opt = case optf of
                            Nothing -> opt1
                            Just (o', []) -> foldl (&) defOpts (o' ++ o)
                            Just (_, e') -> myError $ concat e' ++ "\n" ++ helpMessage
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
              where header = "Usage: listing [Options] [files]\n\n\
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
    trGlobal = applyCols (catMaybes [opts ^. global]) rst
    trExtras = applyCols (opts ^. extraCols) rst
    trMessage = applyCols (catMaybes [opts ^. message]) rst
    allTr = [trKey, trMarks, trGlobal, trExtras, trMessage]
    allRows = map concat . getZipList . sequenceA $
                ZipList . rows <$> allTr
    allNames = concat <$> traverse names allTr
    inds = ColIndices { _keyIndex = 0
                      , _markStart = nFields trKey
                      , _globalIndex = _markStart inds + nFields trMarks
                      , _extrasStart = _globalIndex inds + nFields trGlobal
                      , _messageIndex = _extrasStart inds + nFields trExtras
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

data Formatter = Formatter { _begin :: Text
                           , _end :: Text
                           , _titleLine :: [Text] -> Text
                           , _normalLine :: ColIndices -> Options -> Int -> Row -> Text
                           , _messageLine :: ColIndices -> Options -> Int -> Row -> Text
                           }

makeLenses ''Formatter

hTMLFormatter :: Formatter
hTMLFormatter = Formatter {
    _begin = "<TT><TABLE>"
    , _end = "</TABLE></TT>"
    , _titleLine = htmlTitle
    , _normalLine = htmlLine
    , _messageLine = htmlMessage
}

htmlTitle :: [Text] -> Text
htmlTitle ts = T.concat
         $ "<tr>"
         : map (\t -> "<TH>&nbsp;<B>" <> t <> "</B>&nbsp;</TH>") ts
         ++ ["</tr>"]

fToText :: Int -> Field -> Text
fToText d f
  |  isError f = toString f
  |  otherwise = case typeOf f of
                  TypeInt -> toString f <> "." <> T.replicate d "0"
                  TypeDouble -> T.pack $ showFFloat (Just d) (toDouble f) ""
                  _ -> toString f

colorGlobal :: Options -> Field -> Text
colorGlobal opts f = let
  v = case typeOf f of
         TypeInt -> fromIntegral $ toInt f
         TypeDouble -> toDouble f
         _ -> opts ^. minPass
  in if v < opts ^. canCompensate
     then "red"
     else if v < opts ^. minPass
          then "black"
          else "blue"

trMark :: Int -> Text
trMark n | odd n = "<TR bgcolor=\"#bbbbbb\">"
         | otherwise = "<TR>"

segment :: ColIndices -> Getter ColIndices (Int, Int) -> [a] -> [a]
segment inds g = uncurry slice' (inds ^. g)

htmlLine :: ColIndices -> Options -> Int -> Row -> Text
htmlLine inds opts n r = T.concat
  ( trMark n
  : "<TD>&nbsp;"  -- key
  : toString (r !! (inds ^. keyIndex))
  : "&nbsp;</TD>"
  :  [ "<TD align=\"center\">&nbsp;"
       <> fToText (opts ^. decimals) t
       <> "&nbsp;</TD>"
       | t <- segment inds markInterval r
     ]
  ++ [ "<TD align=\"center\"><font color=\""
          <> colorGlobal opts t <> "\">&nbsp;<b>"
       <> fToText (opts ^. globalDecimals) t
       <> "</font></TD>"
     | t <- segment inds globalInterval r
     ]
  ++ [ "<TD align=\"left\">&nbsp;" <> toString t <> "</TD>"
     | t <- segment inds extrasInterval r
     ]
  ++ [ "</TR>" ]
  )

htmlMessage :: ColIndices -> Options -> Int -> Row -> Text
htmlMessage inds _ n r = T.concat
  ( trMark n
  : "<TD>&nbsp;"  -- key
  : toString (r !! (inds ^. keyIndex))
  : "&nbsp;</TD>"
  : [ "<TD colspan =\"" <> sp
      <> "\"align = \"left\"><font color=\"red\">"
      <> m
      <> "</font></TD></TR>" ]
  )
  where m = toString (r !! (inds ^. messageIndex))
        sp = T.pack $ show $ inds ^. totalCols - 1

laTeXFormatter :: Formatter
laTeXFormatter = Formatter {
    _begin = ""
    , _end = ""
    , _titleLine = laTeXTitle
    , _normalLine = laTeXLine
    , _messageLine = laTeXMessage
}


escapeLaTeX :: Text -> Text
escapeLaTeX = T.concatMap charEscape
    where charEscape '_' = "\\_"
          charEscape '$' = "\\$"
          charEscape '%' = "\\%"
          charEscape '{' = "\\{"
          charEscape '}' = "\\}"
          charEscape '&' = "\\&"
          charEscape '#' = "\\#"
          charEscape '<' = "\\textless"
          charEscape '>' = "\\textgreater"
          charEscape '~' = "\\textasciitilde"
          charEscape '\\' = "\\textbackslash"
          charEscape c = T.singleton c

laTeXTitle :: [Text] -> Text
laTeXTitle = (<> "\\\\\\hline") . T.intercalate " & " . map escapeLaTeX

laTeXLine :: ColIndices -> Options -> Int -> Row -> Text
laTeXLine inds opts n r = T.concat
  ( (if odd n then "\\rowcolor[gray]{0.8}" else "")
  : toString (r !! (inds ^. keyIndex))
  :  [ " & " <> fToText (opts ^. decimals) t
       | t <- segment inds markInterval r
     ]
  ++ [ " & \\textcolor{"
          <> colorGlobal opts t <> "}{"
       <> fToText (opts ^. globalDecimals) t
       <> "}"
     | t <- segment inds globalInterval r
     ]
  ++ [ " & " <> toString t
     | t <- segment inds extrasInterval r
     ]
  ++ [ "\\\\" ]
  )

laTeXMessage :: ColIndices -> Options -> Int -> Row -> Text
laTeXMessage inds _ n r = T.concat
  ( (if odd n then "\\rowcolor[gray]{0.8}" else "")
  : toString (r !! (inds ^. keyIndex))
  : " & "
  : [ "\\multicolumn{" <> sp <> "}{l}{\\qquad\\textcolor{red}{"<> m <> "}}\\\\" ]
  )
  where m = toString (r !! (inds ^. messageIndex))
        sp = T.pack $ show $ inds ^. totalCols - 1

writeListing :: Options -> ColIndices -> RowStore -> IO ()
writeListing opts inds rst
  | opts ^. format == Listatab = do
    let inGlobal = mapCol (fromIntegral $ inds ^. globalIndex)
                          (toField . fToText (opts ^. globalDecimals))
        inMarks = [ mapCol (fromIntegral c)
                           (toField . fToText (opts ^. decimals))
                  | c <- [ inds ^. markStart .. inds ^. markEnd - 1]
                  ]
        rst' = foldl (&) rst (inGlobal : inMarks)
    writeRowStoreStdout (opts ^. oOptions) rst'
  | otherwise = do
    let fmter = case opts ^. format of
                   HTML -> hTMLFormatter
                   LaTeX -> laTeXFormatter
                   Listatab -> error "Impossible. Treated in other case"
    unless (T.null $ fmter ^. begin) $ T.putStrLn $ fmter ^. begin
    unless (ltHeaderType (opts ^. oOptions) == NoHeader) $ do
         let nms = fnames rst
         T.putStrLn $ fmter ^. titleLine $
            concat [ [nms !! (inds ^. keyIndex)]
                   , segment inds markInterval nms
                   , segment inds globalInterval nms
                   , segment inds extrasInterval nms
                   ]
    forM_ (zip [1..] $ rows rst) $ \(n, r) -> do
         let t = toString (r !! (inds ^. messageIndex))
         if inds ^. messageIndex >= nFields rst || T.null t
         then T.putStrLn $ (fmter ^. normalLine) inds opts n r
         else T.putStrLn $ (fmter ^. messageLine) inds opts n r
    unless (T.null $ fmter ^. end) $ T.putStrLn $ fmter ^. end

main :: IO ()
main = do
          opts <- getOptions
          unless (opts ^. format /= Listatab || isNothing (opts ^. message))
            $ myError "There can not be a message column in listatab format"
          rst <- case opts ^. inputFileName of
                     Nothing -> readRowStoreStdin $ opts ^. iOptions
                     Just _ -> load opts
          anonDic <- createAnonDic opts rst
          let (rst', inds) = translate opts anonDic rst
              ind = fromIntegral $ if opts ^. sortByGlobal
                                   then inds ^. globalIndex
                                   else inds ^. keyIndex
              sorted = if opts ^. sortByGlobal || not (opts ^. anonymize)
                       then sortRows ind Ascending rst'
                       else sortRowsOn (T.reverse . toString . (!!! ind)) rst'
          writeListing opts inds sorted

