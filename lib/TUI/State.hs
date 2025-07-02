{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module TUI.State (
    State
    , sRowStore
    , sSourceInfo
    , sIndex
    , sCurrentField
    , sInterface
    , sLog
    , logMessage
    , initialState
    , currentFieldName
    , currentField
    , currentFieldType
    , currentFieldFormula
    , isFormulaCurrentField
    , updateCurrentField
    , changeCurrentFieldFormula
    , changeCurrentFieldName
    , changeCurrentFieldType
    , newRow
    , backward
    , forward
    , fieldBackward
    , fieldForward
    , toggleZoom
    , toggleProperties
    , activateSearch
    , deactivateSearch
    , toggleTable
    , moveTo
    , moveFieldTo
    , moveToSelected
    , moveToSelectedSearch
    , doSave
    , doBackup
    , doFinalBackup
    , searchLetter
) where


import Brick hiding (getName, zoom)
import Brick qualified as B
import Brick.Widgets.Dialog
import Brick.Widgets.List hiding (splitAt, reverse)
import Control.Exception (try, SomeException)
import Control.Lens hiding (index, Zoom, zoom, Level, para)
import Control.Monad (when, void)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import HRowsException (HRowsException(..))
import Model.DefaultFileNames (defaultBackupFileName, defaultConfFileName)
import Model.Expression.RecursionSchemas
import Model.Field
import Model.Row (Row)
import Model.RowStore
import Model.SourceInfo
import System.Directory (removeFile)

import TUI.Base
import TUI.Level
import Data.Char (isLower)

data State = State { _sRowStore :: RowStore
                   , _sSourceInfo :: Maybe (SourceInfo, [SourceInfo])
                   , _sIndex :: Int
                   , _sCurrentField :: Int
                   , _sInterface :: Interface
                   , _sLog :: [Text]
                   }

makeLenses ''State

logMessage :: Text -> EventM Name State ()
logMessage t = sLog %= reverse . take 10 . (t:) . reverse

initialState :: RowStore -> Maybe (SourceInfo, [SourceInfo]) -> State
initialState rst msi = State { _sRowStore = rst
                             , _sSourceInfo = msi
                             , _sIndex = 0
                             , _sCurrentField = 0
                             , _sInterface = In . Back . AsRows $ mkRowViewer rst 0
                             , _sLog = []
                             }

currentFieldName :: State -> Text
currentFieldName s = fnames (s ^. sRowStore) !! (s ^. sCurrentField)

currentField :: State -> Field
currentField s = row (s ^. sIndex) (s ^. sRowStore) !! (s ^. sCurrentField)

currentFieldType :: State -> Text
currentFieldType s = T.pack $ show $ fieldType (fromIntegral $ s ^. sCurrentField) (s ^. sRowStore)

currentFieldFormula :: State -> Maybe Text
currentFieldFormula s = fieldFormula (fromIntegral $ s ^. sCurrentField) (s ^. sRowStore)

isFormulaCurrentField :: State -> Bool
isFormulaCurrentField s = isFormula (fromIntegral $ s ^. sCurrentField) (s ^. sRowStore)

changeCurrentFieldFormula :: Maybe Formula -> EventM Name State ()
changeCurrentFieldFormula mf = do
    f <- use sCurrentField
    sRowStore %= changeFieldFormula mf (fromIntegral f)
    n <- use sIndex
    modify $ moveTo n

updateCurrentField :: Text -> EventM Name State ()
updateCurrentField t = do
    s <- get
    let (rst, ch) = changeField (s ^. sIndex) (fromIntegral $ s ^. sCurrentField) (toField t) (s ^. sRowStore)
    when (notNull ch) $ do
        sRowStore .= rst
        let r = row (s ^. sIndex) rst
        sInterface %= updateLevels (uRow r (r !! (s ^. sCurrentField)))
  where
    uRow :: Row -> Field -> Level Interface -> Level Interface
    uRow _ _ s@(WithDialog _ _) = s
    uRow _ ft (Zoomed (NormalZoom zv) i) = Zoomed (NormalZoom (over zvValue (updateValueViewer ft) zv)) i
    uRow _ _ a@(Back (AsTable _)) = a
    uRow r _ (Back (AsRows rv)) = Back . AsRows $ updateRvValues r rv

replace :: Int -> a -> [a] -> [a]
replace i x xs = take i xs ++ [x] ++ drop (i + 1) xs

changeCurrentFieldName :: Text -> EventM Name State ()
changeCurrentFieldName n = do
    s <- get
    let ns = fnames (s ^. sRowStore)
    when (ns !! (s ^. sCurrentField) /= n) $ do
        let ns' = replace (s ^. sCurrentField) n ns
        sRowStore %= renameFields ns'
        sInterface %= updateLevels (uFieldNames ns' (s ^. sCurrentField))
  where
    uFieldNames :: [Text] -> Int -> Level Interface -> Level Interface
    uFieldNames _  _ s@(WithDialog _ _) = s
    uFieldNames ns cf (Zoomed (NormalZoom zv) i) = Zoomed (NormalZoom (set zvTitle (ns !! cf) zv)) i
    uFieldNames ns _ (Back (AsTable tv)) = Back $ AsTable (set tvFieldNames ns tv)
    uFieldNames ns _ (Back (AsRows tv)) = Back $ AsRows (updateRvNames ns tv)

changeCurrentFieldType :: Text -> EventM Name State ()
changeCurrentFieldType t = do
    s <- get
    let nt = read $ T.unpack t
    ts <- uses sRowStore types
    when (ts  !! (s ^. sCurrentField) /= nt) $
        sRowStore %= changeFieldType nt (fromIntegral $ s ^. sCurrentField)

newRow :: EventM Name State ()
newRow = do
    B.zoom sRowStore $ modify addEmptyRow
    s <- uses sRowStore size
    modify $ moveTo (s - 1)

notNull :: [a] -> Bool
notNull [] = False
notNull _ = True


backward :: EventM Name State ()
backward = uses sIndex (subtract 1) >>= modify . moveTo

forward :: EventM Name State ()
forward = uses sIndex (+ 1) >>= modify . moveTo

fieldBackward :: EventM Name State ()
fieldBackward = uses sCurrentField (subtract 1) >>= modify . moveFieldTo

fieldForward :: EventM Name State ()
fieldForward = uses sCurrentField (+ 1) >>= modify . moveFieldTo

toggleZoom :: EventM Name State ()
toggleZoom = use (sInterface . levelZoom) >>= \case
                Just _ -> sInterface . levelZoom .= Nothing
                Nothing -> modify zoom

zoomViewerFromState :: State -> ZoomViewer
zoomViewerFromState s = mkZoomViewer (currentFieldName s)
                                      (isFormulaCurrentField s)
                                      (currentField s)

zoom :: State -> State
zoom s = set (sInterface . levelZoom) (Just . NormalZoom $ zoomViewerFromState s) s

toggleProperties :: EventM Name State ()
toggleProperties = use (sInterface . fieldProperties) >>= \case
                    Just _ -> sInterface . fieldProperties .= Nothing
                    Nothing -> modify properties

properties :: State -> State
properties s = set (sInterface . fieldProperties) (Just $ mkFieldPropertiesDialog (currentFieldName s)
                                            (isFormulaCurrentField s)
                                            (currentField s)
                                            (currentFieldType s)
                                            (currentFieldFormula s)) s

activateSearch :: EventM Name State ()
activateSearch = do
   index <- use sCurrentField
   tle <- (!! index) <$> uses sRowStore fnames
   vs <- uses sRowStore (fieldValues (fromIntegral index))
   sInterface . searchDialog .= Just (mkSearchDialog SearchList maxWidth tle vs)


deactivateSearch :: EventM Name State ()
deactivateSearch = sInterface . searchDialog .= Nothing


toggleTable :: EventM Name State ()
toggleTable = do
    rst <- use sRowStore
    idx <- use sIndex
    fld <- use sCurrentField
    use (sInterface . tableViewer) >>= \case
                Just _ -> do
                            sInterface . rowViewer .= Just (mkRowViewer rst idx)
                            modify $ moveFieldTo fld
                Nothing -> do
                             sInterface . tableViewer .= Just (buildTable rst idx)
                             modify $ moveTo idx
                             modify $ moveFieldTo fld

moveToSelected :: EventM Name State ()
moveToSelected = do
    msd <- use $ sInterface . searchDialog
    let ds = do
               sd <- msd
               dialogSelection (sd ^. sdDialog)
    case ds of
        Just (DButton OkButton, ()) -> do
            deactivateSearch
            let se = do
                       sd <- msd
                       listSelectedElement (sd ^. sdValues)
            case se of
                Nothing -> return ()
                Just (_, t) -> do
                    s <- get
                    let pos = nextPos (fromIntegral $ s ^. sCurrentField) t (s ^. sIndex) (s ^. sRowStore)
                    modify $ moveTo pos
        Just (DButton CancelButton, ()) -> deactivateSearch
        _ -> return ()

moveToSelectedSearch :: Int -> EventM Name State ()
moveToSelectedSearch r = do
    msd <- use $ sInterface . searchDialog
    case msd of
        Nothing -> return ()
        Just sd -> do
            deactivateSearch
            let vs = sd ^. sdValues . listElementsL
            if r < 0 || r >= V.length vs
            then return ()
            else do
                let t = vs V.! r
                s <- get
                let pos = nextPos (fromIntegral $ s ^. sCurrentField) t (s ^. sIndex) (s ^. sRowStore)
                modify $ moveTo pos

searchLetter :: Char -> EventM Name State ()
searchLetter c = do
    msd <- use $ sInterface . searchDialog
    case msd of
        Nothing -> return ()
        Just sd -> do
            let vs = sd ^. sdValues . listElementsL
                mi = case V.findIndex (T.isPrefixOf (T.singleton c)) vs of
                         Nothing -> if isLower c
                                    then V.findIndex (T.isPrefixOf (T.toUpper (T.singleton c))) vs
                                    else V.findIndex (T.isPrefixOf (T.toLower (T.singleton c))) vs
                         Just i -> Just i
            case mi of
                Nothing -> return ()
                Just i -> sInterface . searchDialog . _Just . sdValues %= listMoveTo i

moveTo :: Int -> State -> State
moveTo pos s
  | valid = over sInterface moveInterface indexUpdated
  | otherwise = s
  where valid = 0 <= pos && pos < size (s ^. sRowStore)
        indexUpdated = set sIndex pos s
        moveInterface = updateLevels mi
        mi se@(WithDialog _ _) = se
        mi (Zoomed (NormalZoom zv) i) = Zoomed (NormalZoom (updateZoomViewer (currentField indexUpdated) zv)) i
        mi (Back (AsTable tv)) = Back $ AsTable (over tvLists (listMoveTo pos) tv)
        mi (Back (AsRows rv)) = let
                           vList = case listSelected (rv ^. rvFieldNames) of
                                     Nothing -> valueList pos (s ^. sRowStore)
                                     Just n -> listMoveTo n (valueList pos (s ^. sRowStore))
                         in Back $ AsRows (set rvValueList vList rv)


moveFieldTo :: Int -> State -> State
moveFieldTo pos s
  | valid = over sInterface moveInterface posUpdated
  | otherwise = s
    where valid = 0 <= pos && pos < length (fnames $ s ^. sRowStore)
          posUpdated = set sCurrentField pos s
          moveInterface = updateLevels mi
          mi (WithDialog dl i) = WithDialog dl i
          mi (Zoomed (NormalZoom _) i) = Zoomed (NormalZoom $ zoomViewerFromState posUpdated) i
          mi (Back (AsTable tv)) = Back $ AsTable (set tvCurrentField pos tv)
          mi (Back (AsRows rv)) = Back $ AsRows (over rvFieldNames (listMoveTo pos) $
                                        over rvValueList (listMoveTo pos) rv)

doSave :: EventM Name State ()
doSave = do
    msi <- use sSourceInfo
    case msi of
        Nothing -> logMessage "No source info available"
        Just (si, sis) -> do
            rst <- use sRowStore
            let pc = siPathAndConf si
                si' = case confPath pc of
                         Nothing -> let
                                       cnf = defaultConfFileName $ path pc
                                    in changePathAndConf (pc { confPath = Just cnf }) si
                         Just _ -> si
            liftIO $ writeRowStore si' sis rst
            sRowStore %= setUnchanged

doBackup :: EventM Name State ()
doBackup = use sRowStore >>= (\case
    False -> return ()
    True -> do
        msi <- use sSourceInfo
        case msi of
            Nothing -> return ()
            Just (si, sis) -> do
                let conf = defaultBackupFileName <$> confPath (siPathAndConf si)
                    fp = defaultBackupFileName $ path (siPathAndConf si)
                    si' = changePathAndConf (PathAndConf fp conf) si
                rst <- use sRowStore
                (liftIO . try $ writeRowStore si' sis rst) >>= \case
                    Right _ -> return ()
                    Left (HRowsException e) -> logMessage ("Backup error: " <> e))
        . changed

doFinalBackup :: EventM Name State ()
doFinalBackup = use sRowStore >>= (\case
    True -> doBackup
    False -> do
        msi <- use sSourceInfo
        case msi of
            Nothing -> return ()
            Just (si, _) -> do
                let conf = defaultBackupFileName <$> confPath (siPathAndConf si)
                    fp = defaultBackupFileName $ path (siPathAndConf si)
                void . liftIO $ ((try $ do
                                    removeFile fp
                                    maybe (return ()) removeFile conf) :: IO (Either SomeException ())))
        . changed

