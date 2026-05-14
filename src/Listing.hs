{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

import Control.Applicative(ZipList(..), Alternative ((<|>)))
import Control.Arrow((&&&))
import Control.Monad(unless, when, forM_)
import Control.Lens
import Data.Default(Default(def))
import Data.List(intercalate, sortOn)
import Data.Map(Map)
import Data.Map qualified as M
import Data.Maybe(catMaybes, isNothing, fromMaybe, fromJust)
import Data.Text(Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Environment(getArgs)
import System.Exit(exitSuccess)
import System.IO(IOMode (ReadMode), openFile)

import System.Console.JMVOptions

import Col
import Model.Row
import Model.RowStore
import Model.SourceInfo
import Numeric (showFFloat)
import Model.RowStore.RowStoreConf (fromNamesTypes)

data Format = HTML | LaTeX | Listatab deriving (Show, Read, Enum, Eq)

type AnonDic = Map Text Text

data ColIndices = ColIndices { _keyIndex :: Int
                             , _markStart :: Int
                             , _globalIndex :: Int
                             , _extrasStart :: Int
                             , _extrasEnd :: Int
                             , _messageIndex :: Maybe Int
                             } deriving Show
makeLenses ''ColIndices

markEnd :: Getter ColIndices Int
markEnd = globalIndex

markInterval :: Getter ColIndices (Int, Int)
markInterval = to $ view markStart &&& view markEnd

extrasInterval :: Getter ColIndices (Int, Int)
extrasInterval = to $ view extrasStart &&& view extrasEnd

globalEnd :: Getter ColIndices Int
globalEnd = extrasStart

globalInterval :: Getter ColIndices (Int, Int)
globalInterval = to $ view globalIndex &&& view globalEnd

data Options = Options { _anonymize :: Bool
                       , _anonFile :: Maybe FilePath
                       , _anonKey :: Col
                       , _anonLength :: Int
                       , _format :: Format
                       , _minPass :: Double
                       , _canCompensate :: Double
                       , _sortByGlobal :: Bool

                       , _key :: Col
                       , _marks :: ColSpec
                       , _decimals :: Int
                       , _global :: Maybe Col
                       , _globalDecimals :: Int
                       , _message :: Maybe Col
                       , _messageExtension :: Maybe Int
                       , _extraCols :: ColSpec

                       , _optionsFile :: Maybe FilePath
                       , _cOptions :: ColOptions
                       }

makeLenses ''Options

instance Default Options where
    def = Options { _anonymize = False
                  , _anonFile = Nothing
                  , _anonKey = Single (mkPosition 0) Nothing
                  , _anonLength = 5
                  , _format = LaTeX
                  , _minPass = 5
                  , _canCompensate = 4
                  , _sortByGlobal = False

                  , _key = Single (mkPosition 0) Nothing
                  , _marks = SelectedCols []
                  , _decimals = 2
                  , _global = Nothing
                  , _globalDecimals = 1
                  , _message = Nothing
                  , _messageExtension = Nothing
                  , _extraCols = SelectedCols []

                  , _optionsFile = Nothing
                  , _cOptions = def
                  }

setSingleCol :: Traversal' Options Col -> String -> String -> Options -> Options
setSingleCol l n s = case parseCols (T.pack s) of
                       Left e -> myError $ "Bad column especification in " ++ n ++ ": " ++ T.unpack e
                       Right (SelectedCols [c@(Single _ _)]) -> set l c
                       Right _ -> myError $ "For " ++ n ++ " you have to specify exactly one column"

setMaybeCol :: Lens' Options (Maybe Col) -> String -> String -> Options -> Options
setMaybeCol l n s = setSingleCol (l . _Just) n s . set l (Just $ def ^. anonKey)

defValue :: Show a => Lens' Options a -> String
defValue l = "Default: " ++ show (def ^. l) ++ "."

options :: [OptDescr (Options -> Options)]
options =  colOptions FullIOOptions cOptions ++
           processOptions (do
               'o' ~: "optionsFile" ==> ReqArg (set optionsFile . Just) "FILE" ~: "Read the options from a file. Each line in the file has a long option. If there is a parameter, it is written after a colon."
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
               'e' ~: "messageExtension" ==> ReqArg (set messageExtension . Just . read) "INT" ~: "Number of columns overriden by the message. Default: all columns."
               'x' ~: "extraCols" ==> ReqArg (appendCols extraCols "extraCols") "COLS" ~: "Columns with additional information. May apper more than once. Default: no extras."
               'd' ~: "decimals" ==> ReqArg (set decimals . read) "DECS" ~: "Number of decimal places. " ++ defValue decimals
               'D' ~: "globalDecimals" ==> ReqArg (set globalDecimals . read) "DECS" ~: "Number of decimal places of the global mark. " ++ defValue globalDecimals
               'p' ~: "minPass" ==> ReqArg (set minPass . read) "MARK" ~: "Minimum passing mark. " ++ defValue minPass
               'P' ~: "canCompensate" ==> ReqArg (set canCompensate . read) "MARK" ~: "Minimum mark that can be compensated. " ++ defValue canCompensate
               'F' ~: "format" ==> ReqArg (set format . read) "FORMAT" ~: "Format of the output, one of " ++ showEnum HTML ++ ". " ++ defValue format
               'G' ~: "sortByGlobal" ==> NoArg (set sortByGlobal True) ~: "Sort using the global column instead of the key"
               )

showEnum :: (Enum a, Show a) => a -> String
showEnum = intercalate ", " . map show . enumFrom

getOptions :: IO Options
getOptions = do
                args <- getArgs
                let (o, a, e) = getOpt Permute options args
                let opt1 = foldl (&) def o
                when (opt1 ^. cOptions . help) $ putStrLn helpMessage >> exitSuccess
                unless (null e) $ myError $ concat e ++ "\n" ++ helpMessage
                optf <- traverse  (\f -> openFile f ReadMode >>= optionsFromHandle options)
                                 (opt1 ^. optionsFile)
                let opt = case optf of
                            Nothing -> opt1
                            Just (o', []) -> foldl (&) def (o' ++ o)
                            Just (_, e') -> myError $ concat e' ++ "\n" ++ helpMessage
                return $ case a of
                   [] -> opt
                   [f] -> set (cOptions . inputFileName) (Just f) opt
                   [f, c] -> set (cOptions . inputFileName) (Just f)
                           $ set (cOptions . confFileName) (Just c) opt
                   _ -> myError "Too many filenames"


helpMessage :: String
helpMessage = usageInfo header options
              where header = "Usage: listing [Options] [files]\n\n\
                             \Creates a listing of marks from a listatab file.\n\
                             \Columns can be specified by formulas using hrows syntax.\n\
                             \Options receiving a list of formulas can list them \
                             \separated\n\
                             \by commas and also as ranges like [$1:$3] or [Q1:Q4]."

translate :: Options -> Maybe AnonDic -> RowStore -> (RowStore, ColIndices)
translate opts mdic rst = let
    allCols = singleCol (opts ^. key)
              <> (opts ^. marks)
              <> (SelectedCols $ catMaybes [opts ^. global])
              <> (opts ^. extraCols)
              <> (SelectedCols $ catMaybes [opts ^. message])
    inds = ColIndices { _keyIndex = 0
                      , _markStart = 1
                      , _globalIndex = _markStart inds + specLength rst (opts ^. marks) 
                      , _extrasStart = _globalIndex inds + maybe 0 (const 1) (opts ^. global)
                      , _extrasEnd = _extrasStart inds + specLength rst (opts ^. extraCols)
                      , _messageIndex = const (_extrasEnd inds) <$> opts ^. message
                      }
    rst' = applyCols allCols rst
  in case mdic of
       Nothing -> (rst', inds)
       Just dic -> let
                     mkAnon = toField . (dic M.!) . toString
                   in (mapCol (fromIntegral $ inds ^. keyIndex) mkAnon rst', inds)

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
            let readAction = do
                               pc <- mkPathAndConf f Nothing
                               let sinfo = mkSourceInfo Nothing pc def
                               readRowStore sinfo
            return (keys (opts ^. anonKey) . fst <$> readAction)
          fromRst = if opts ^. anonymize
                    then Just . return $ keys (opts ^. key) rst
                    else Nothing

data CellType = KeyCell | MarkCell | GlobalCell | ExtraCell | MessageCell Int deriving (Show, Eq)

data Formatter = Formatter { _begin :: Text
                           , _end :: Text
                           , _titleLine :: [Text] -> Text
                           , _cellFormatter :: Options -> (CellType, Field) -> Text
                           , _lineFormatter :: Options -> Int -> [Text] -> Text
                           }

makeLenses ''Formatter

hTMLFormatter :: Formatter
hTMLFormatter = Formatter {
    _begin = "<TT><TABLE>"
    , _end = "</TABLE></TT>"
    , _titleLine = htmlTitle
    , _cellFormatter = htmlCell
    , _lineFormatter = htmlLine
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
                  TypeInt -> case d of
                                0 -> toString f
                                _ -> toString f <> "." <> T.replicate d "0"
                  TypeDouble -> T.pack $ showFFloat (Just d) (toDouble f) ""
                  _ -> toString f

colorGlobal :: Options -> Field -> Text
colorGlobal _ f | isError f = "black"
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

htmlCell :: Options -> (CellType, Field) -> Text
htmlCell _ (KeyCell, f) = "<TD>&nbsp;" <> toString f <> "&nbsp;</TD>"
htmlCell opts (MarkCell, f) = "<TD align=\"center\">&nbsp;"
                            <> fToText (opts ^. decimals) f
                            <> "&nbsp;</TD>"
htmlCell opts (GlobalCell, f) = "<TD align=\"center\"><font color=\""
                            <> colorGlobal opts f <> "\">&nbsp;<b>"
                            <> fToText (opts ^. globalDecimals) f
                            <> "</font></TD>"
htmlCell _ (ExtraCell, f) = "<TD align=\"left\">&nbsp;" <> toString f <> "</TD>"
htmlCell _ (MessageCell c, f) = "<TD colspan =\"" <> T.pack (show c)
                            <> "\"align = \"left\"><font color=\"red\">"
                            <> toString f
                            <> "</font></TD>"

htmlLine :: Options -> Int -> [Text] -> Text
htmlLine _ n ts = T.concat
    ( trMark n
    : ts ++ [ "</TR>" ]
    )

laTeXFormatter :: Formatter
laTeXFormatter = Formatter {
    _begin = ""
    , _end = ""
    , _titleLine = laTeXTitle
    , _cellFormatter = laTeXCell
    , _lineFormatter = laTeXLine
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

laTeXCell :: Options -> (CellType, Field) -> Text
laTeXCell _ (KeyCell, f) = escapeLaTeX $ toString f
laTeXCell opts (MarkCell, f) = escapeLaTeX $ fToText (opts ^. decimals) f
laTeXCell opts (GlobalCell, f) = "\\textcolor{" <> colorGlobal opts f <> "}{"
                                 <> escapeLaTeX (fToText (opts ^. globalDecimals) f)
                                 <> "}"
laTeXCell _ (ExtraCell, f) = escapeLaTeX $ toString f
laTeXCell _ (MessageCell c, f) = "\\multicolumn{" <> T.pack (show c)
                                 <> "}{l}{\\qquad\\textcolor{red}{" <> escapeLaTeX (toString f) <> "}}}"


laTeXLine :: Options -> Int -> [Text] -> Text
laTeXLine _ n ts = T.concat
  [ if odd n then "\\rowcolor[gray]{0.8}" else ""
  , T.intercalate " & " ts
  , "\\\\"
  ]


collectCells :: Options -> ColIndices -> Row -> [(CellType, Field)]
collectCells opts inds r = let
   fm = (r !!) <$> (inds ^. messageIndex)
   sm = toString <$> fm
   me = fromMaybe (inds ^. extrasEnd - 1) (opts ^. messageExtension)
   cells = (KeyCell, r !! (inds ^. keyIndex))
           : [ (MarkCell, t) | t <- segment inds markInterval r ]
           ++ [ (GlobalCell, t) | t <- segment inds globalInterval r ]
           ++ [ (ExtraCell, t) | t <- segment inds extrasInterval r ]
 in if (T.null <$> sm) /= Just False
    then cells
    else cells !! 0 : (MessageCell me, fromJust fm) : drop (me + 1) cells

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
    writeRowStoreStdout (opts ^. cOptions . oOptions) rst'
  | otherwise = do
    let fmter = case opts ^. format of
                   HTML -> hTMLFormatter
                   LaTeX -> laTeXFormatter
                   Listatab -> error "Impossible. Treated in other case"
    unless (T.null $ fmter ^. begin) $ T.putStrLn $ fmter ^. begin
    unless (ltHeaderType (opts ^. cOptions . oOptions) == NoHeader) $ do
         let nms = fnames rst
         T.putStrLn $ fmter ^. titleLine $
            concat [ [nms !! (inds ^. keyIndex)]
                   , segment inds markInterval nms
                   , segment inds globalInterval nms
                   , segment inds extrasInterval nms
                   ]
    forM_ (zip [1..] $ rows rst) $ \(n, r) -> do
         let cells = collectCells opts inds r
             texts = map (fmter ^. cellFormatter $ opts) cells
             line = (fmter ^. lineFormatter) opts n texts
         T.putStrLn line
    unless (T.null $ fmter ^. end) $ T.putStrLn $ fmter ^. end

main :: IO ()
main = do
          opts <- getOptions
          unless (opts ^. format /= Listatab || isNothing (opts ^. message))
            $ myError "There can not be a message column in listatab format"
          rst <- readRowStoreFromOptions $ opts ^. cOptions
          anonDic <- createAnonDic opts rst
          let (rst', inds) = translate opts anonDic rst
              ind = fromIntegral $ if opts ^. sortByGlobal
                                   then inds ^. globalIndex
                                   else inds ^. keyIndex
              sorted = if opts ^. sortByGlobal || not (opts ^. anonymize)
                       then sortRows ind Ascending rst'
                       else sortRowsOn (T.reverse . toString . (!!! ind)) rst'
          writeListing opts inds sorted

