{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Col (
    -- *Types
    ColSpec(..)
    , Col(..)
    -- *Functions
    , readRowStoreFromOptions
    , readRowStoreAndSourceInfo
    , parseCols
    , applyCols
    , slice
    , slice'
    , pos
    -- *Lenses
    , expressionT
    , colF
    -- *Option processing
    , ColOptions
    , IOOptions(..)
    , colOptions
    , setCols
    , appendCols
    , help
    , iOptions
    , oOptions
    , rFilter
    , inputFileName
    , confFileName
    , myError
) where

import Control.Lens (Traversal', (%~), (&), Fold, folding, makeLenses, Lens', over, set, (^.))
import Data.Default ( def, Default(def) )
import Data.Text (Text)
import Data.Text qualified as T

import System.Console.JMVOptions
import Model.Row
import Model.RowStore
import Model.Expression.Evaluation
import Model.Expression.Manipulate
import Model.Expression.Lexer (Token(EOFT, CommaT, ColonT, OpenSBT, CloseSBT, StringT))
import Model.Expression.Parser
import Model.Expression.RecursionSchemas
import Model.SourceInfo
import System.Environment (getProgName)
import System.Exit (exitFailure)
import System.IO.Unsafe (unsafePerformIO)
import System.IO (hPutStrLn, stderr)
import Control.Monad (when)
import qualified Control.Exception as E
import HRowsException (HRowsException(HRowsException))
import Control.Monad.State (execState, gets, modify)
import Model.RowStore.RowStoreConf (RowStoreConf (sourceInfos))

-- |A Col especifies a column of the input in the command line. Single
-- expressions especify a column, a couple of expressions that correspond
-- each to a column, especify a range. 
data Col = Single Expression (Maybe Text)
         | Range Expression Expression deriving Show

-- | A ColSpec is either all columns or a list of `Col`.
data ColSpec = AllCols | SelectedCols [Col] deriving Show

-- |Options for cols like programs

data ColOptions = ColOptions { _help :: Bool
                             , _iOptions :: ListatabInfo
                             , _oOptions :: ListatabInfo
                             , _rFilter :: Maybe Expression
                             , _sortExpr :: Maybe Expression
                             , _inputFileName :: Maybe FilePath
                             , _confFileName :: Maybe FilePath
                             }

makeLenses ''ColOptions

-- |Used to specify whether the program accepts options for formatting
-- the output.
data IOOptions = OnlyInputOptions | FullIOOptions | OnlyInputNoFilterOptions deriving Eq

instance Default ColOptions where
    def = ColOptions { _help = False
                     , _iOptions = def
                     , _oOptions = def
                     , _rFilter = Nothing
                     , _sortExpr = Nothing
                     , _inputFileName = Nothing
                     , _confFileName = Nothing
                     }
--
-- |A traversal of the expressions in the `Col`
expressionT :: Traversal' Col Expression
expressionT f (Single e mn) = Single <$> f e <*> pure mn
expressionT f (Range e1 e2) = Range <$> f e1 <*> f e2

-- |A fold of the Fields specified by a `Col`
colF :: Col -> Fold RowStore Row
colF col = folding getRows
    where getRows rst = map (processRow dss $ SelectedCols [col']) $ rows rst
             where col' = col & expressionT %~ addPositions rst
                   dss = getDataSources rst



-- Parses a String to a Char representing a separator. Recongizes only
-- strings with one char or with a scape followed by a t.
parseSeparator :: String -> Char
parseSeparator [c] = c
parseSeparator "\\t" = '\t'
parseSeparator s = error $ "Illegal string for separator: " ++ show s

setSeparator :: Lens' ColOptions ListatabInfo -> Char -> ColOptions -> ColOptions
setSeparator l s = over l (\oc -> oc { ltSeparator = s })

setHeader :: Lens' ColOptions ListatabInfo -> HeaderType -> ColOptions -> ColOptions
setHeader l c = over l (\oc -> oc { ltHeaderType = c })

-- |Parse a String to extract a list of cols and set it
setCols :: Lens' o ColSpec -> String -> String -> o -> o
setCols l n s = case parseCols (T.pack s) of
                      Left e -> myError $ "Bad column especification in " ++ n ++ ": " ++ T.unpack e
                      Right cs -> set l cs

-- |Parse a String to extract a list of cols and append them
appendCols :: Lens' o ColSpec -> String -> String -> o -> o
appendCols l n s = case parseCols (T.pack s) of
                      Left e -> myError $ "Bad column especification in " ++ n ++ ": " ++ T.unpack e
                      Right (SelectedCols cs) -> over l (\(SelectedCols cs0) -> SelectedCols (cs0 ++ cs))
                      Right AllCols -> myError $ "Cannot append all columns in " ++ n

setFilter :: String -> ColOptions -> ColOptions
setFilter s = case parse expression $ T.pack s of
                   Left e -> myError $ "Bad expression in the filter: " ++ T.unpack e
                   Right ex -> set rFilter $ Just ex

setSort :: String -> ColOptions -> ColOptions
setSort s = case parse expression $ T.pack s of
                   Left e -> myError $ "Bad expression in the sort criterion: " ++ T.unpack e
                   Right ex -> set sortExpr $ Just ex

colOptions :: IOOptions -> Lens' o ColOptions -> [OptDescr (o -> o)]
colOptions io l = map (fmap $ over l) . processOptions $ do
               '0' ~: "iNoHeader" ==> NoArg (setHeader iOptions NoHeader . setHeader oOptions NoHeader) ~: "Do not use header in the input."
               when (io == FullIOOptions) $
                   'O' ~: "oNoHeader" ==> NoArg (setHeader oOptions NoHeader) ~: "Do not use header in the output. Must be used after -0 if both are present."
               '1' ~: "iHeader1" ==> NoArg (setHeader iOptions FirstLine . setHeader oOptions FirstLine) ~: "Use the first line as header in the input."
               when (io == FullIOOptions) $
                   'H' ~: "oHeader1" ==> NoArg (setHeader oOptions FirstLine) ~: "Use the first line as header in the output"
               when (io /= OnlyInputNoFilterOptions) $ do
                   'f' ~: "filter" ==> ReqArg setFilter "FILTER" ~: "An integer expression that will be used to filter the rows. Those for which the result is greater than 0"
                   'R' ~: "sort" ==> ReqArg setSort "EXPR" ~: "Use this expression to sort the rows. (Default: do no sort)"
               'h' ~: "help" ==> NoArg (set help True) ~: "This help."
               's' ~: "separator" ==> ReqArg (\s -> let c = parseSeparator s in setSeparator iOptions c . setSeparator oOptions c) "CHAR" ~:
                        ("Field separator for the input and output. (Default: " ++ show (ltSeparator $ def ^. iOptions) ++ ").")
               when (io == FullIOOptions) $
                   'S' ~: "oSeparator" ==> ReqArg (setSeparator oOptions . parseSeparator) "CHAR" ~:
                        "Field separator for the output. (Default: same as -s). Must appear after -s when both are present."


myError :: String -> a
myError m = unsafePerformIO $ do
              n <- getProgName
              hPutStrLn stderr $ n ++ " error: " ++ m
              exitFailure

-- |Parse a list of expressions separated by commas, return
-- the corresponding `ColSpec` or an error message
parseCols :: Text -> Either Text ColSpec
parseCols = fmap SelectedCols <$> parse colParser

colParser :: Parser [Col]
colParser = do
    c <- current >>= \case
             OpenSBT -> rangeParser
             _ -> do
                    e <- expression
                    check OpenSBT >>= \case
                        False -> return $ Single e Nothing
                        True -> do
                            advance
                            current >>= \case
                                StringT s -> do
                                    advance
                                    expect CloseSBT "a closing square bracket"
                                    return $ Single e (Just $ T.pack s)
                                t -> parsingError $ T.concat ["Expected a name for the column, found a "
                                                             , T.pack $ show t]

    check EOFT >>= \case
        True -> return [c]
        False -> expect CommaT "a comma" >> (c:) <$> colParser

rangeParser :: Parser Col
rangeParser = do
    advance
    e1 <- expression
    checkPosition e1
    expect ColonT "a colon"
    e2 <- expression
    checkPosition e2
    expect CloseSBT "a closing square bracket"
    return $ Range e1 e2

checkPosition :: Expression -> Parser ()
checkPosition (In (Position _)) = return ()
checkPosition (In (NamedPosition _ _)) = return ()
checkPosition e = parsingError $ T.concat [ "Expression "
                                          , toFormula e
                                          , " does not represent a position"
                                          ]

-- |Produce a `RowStore` from a `ColSpec` and an existing
-- `RowStore`.
applyCols :: ColSpec -> RowStore -> RowStore
applyCols AllCols rst = rst
applyCols (SelectedCols cs0) rst = setChanged $ mkRSFromExpressions expNames rst
    where expNames = concatMap toExpName cs0
          toExpName (Single e Nothing) = [(e, toFormula e)]
          toExpName (Single e (Just n)) = [(e, n)]
          toExpName (Range e1 e2) = slice (pos e1) (pos e2) defExpNames
          defExpNames = do
            n <- fnames rst
            return (In $ NamedPosition n Nothing, n)

processRow :: [DataSource] -> ColSpec -> Row -> Row
processRow _ AllCols r = r
processRow dss (SelectedCols cs) r = concatMap f cs
    where f (Single e _) = [evaluate r dss e]
          f (Range e1 e2) = slice (pos e1) (pos e2) r

-- The elements of the list from p1 to p2, both included
slice :: Int -> Int -> [a] -> [a]
slice p1 p2 = take (p2 - p1 + 1) . drop p1

-- The elements of the list from p1 to p2, p2 excluded
slice' :: Int -> Int -> [a] -> [a]
slice' p1 p2 = take (p2 - p1) . drop p1

pos :: Expression -> Int
pos (In (Position n)) = n
pos (In (NamedPosition _ (Just n))) = n
pos e = error $ "Expression "
                ++ T.unpack (toFormula e)
                ++ " does not represent a position"

load :: ColOptions -> IO (RowStore, Maybe (SourceInfo, [SourceInfo]))
load opts = case opts ^. inputFileName of
    Nothing -> (, Nothing) <$> readRowStoreStdin (opts ^. iOptions)
    Just fn -> do
         pc <- mkPathAndConf fn $ opts ^. confFileName
         let sinfo =  mkSourceInfo Nothing pc $ opts ^. iOptions

         r <- E.try $ readRowStore sinfo
         case r of
             Right (rst, mconf) -> case mconf of
                 Nothing -> return (rst, Just (sinfo, []))
                 Just cnf -> return (rst, Just (sinfo, sourceInfos cnf))
             Left (HRowsException mess) -> myError $ T.unpack mess

-- |Uses the options to read a `RowStore`
readRowStoreFromOptions :: ColOptions -> IO RowStore
readRowStoreFromOptions opts = fst <$> readRowStoreAndSourceInfo opts


-- |Uses the options to build the `SourceInfo` and read a `RowStore`
readRowStoreAndSourceInfo :: ColOptions -> IO (RowStore, Maybe (SourceInfo, [SourceInfo]))
readRowStoreAndSourceInfo opts = do
  (rst, msinfo) <- load opts
  return . (, msinfo) . flip execState rst $ do
        case opts ^. rFilter of
             Nothing -> return ()
             Just e -> do
                          dss <- gets getDataSources
                          ex <- gets $ flip addPositions e
                          modify . filterRows $ (> 0) . toInt . (\r -> evaluate r dss ex)
        case opts ^. sortExpr of
             Nothing -> return ()
             Just e -> do
                          dss <- gets getDataSources
                          ex <- gets $ flip addPositions e
                          modify . sortRowsOn $ (\r -> evaluate r dss ex)

