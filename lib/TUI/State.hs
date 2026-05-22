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
    , undo
    , redo
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
    , doSave
    , doBackup
    , doFinalBackup
) where


import Brick hiding (getName, zoom)
import Brick.Widgets.List hiding (splitAt, reverse)
import Control.Exception (try, SomeException)
import Control.Lens hiding (index, Zoom, zoom, Level, para)
import Control.Monad (when, void, unless)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import System.Directory (removeFile)

import HRowsException (HRowsException(..))
import Model.DefaultFileNames (defaultBackupFileName, defaultConfFileName)
import Model.Expression.RecursionSchemas
import Model.Field
import Model.Row (Row)
import Model.RowStore
import Model.SourceInfo
import Model.UndoZipper qualified as U
import TUI.Base
import TUI.Level

type UZipper = U.UndoZipper (RowStore, RowPos)

data State = State { _sRowStoreZipper :: UZipper
                   , _sSourceInfo :: Maybe (SourceInfo, [SourceInfo])
                   , _sIndex :: RowPos
                   , _sCurrentField :: Int
                   , _sInterface :: Interface
                   , _sLog :: [Text]
                   }

makeLenses 'State

sRowStore :: Getter State RowStore
sRowStore = to (fst . U.current . _sRowStoreZipper)

setSRowStore :: Setter' State RowStore
setSRowStore = sets $ \f s -> over sRowStoreZipper (U.push (f $ s ^. sRowStore, s ^. sIndex)) s

logMessage :: Text -> EventM Name State ()
logMessage t = sLog %= reverse . take 10 . (t:) . reverse
-- logMessage _ = return ()

initialState :: RowStore -> Maybe (SourceInfo, [SourceInfo]) -> State
initialState rst msi = State { _sRowStoreZipper = U.mkUndoZipper (rst, index)
                             , _sSourceInfo = msi
                             , _sIndex = index
                             , _sCurrentField = 0
                             , _sInterface = In . Back . AsRows $ mkRowViewer rst 0
                             , _sLog = []
                             }
    where index = if size rst > 0 then 0 else -1


currentFieldName :: State -> Text
currentFieldName s = fnames (s ^. sRowStore) !! (s ^. sCurrentField)

currentField :: State -> Field
currentField s = row (s ^. sIndex) (s ^. sRowStore) !! (s ^. sCurrentField)

currentFieldType :: State -> FieldType
currentFieldType s = fieldType (fromIntegral $ s ^. sCurrentField) (s ^. sRowStore)

currentFieldFormula :: State -> Maybe Text
currentFieldFormula s = fieldFormula (fromIntegral $ s ^. sCurrentField) (s ^. sRowStore)

isFormulaCurrentField :: State -> Bool
isFormulaCurrentField s = isFormula (fromIntegral $ s ^. sCurrentField) (s ^. sRowStore)


undo :: EventM Name State Bool
undo = moveZipper U.back

redo :: EventM Name State Bool
redo = moveZipper U.forward

moveZipper :: (UZipper -> Maybe UZipper) -> EventM Name State Bool
moveZipper move = do
         zp <- use sRowStoreZipper
         case move zp of
              Nothing -> return False
              Just zp' -> do
                            sRowStoreZipper .= zp'
                            modify . moveTo . snd $ U.current zp'
                            return True


changeCurrentFieldFormula :: Maybe Formula -> EventM Name State ()
changeCurrentFieldFormula mf = do
    f <- use sCurrentField
    setSRowStore %= changeFieldFormula mf (fromIntegral f)
    n <- use sIndex
    modify $ moveTo n

updateCurrentField :: Field -> EventM Name State ()
updateCurrentField f = do
    s <- get
    let (rst, ch) = changeField (s ^. sIndex) (fromIntegral $ s ^. sCurrentField) f (s ^. sRowStore)
    when (notNull ch) $ do
        setSRowStore .= rst
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
        setSRowStore %= renameFields ns'
        sInterface %= updateLevels (uFieldNames ns' (s ^. sCurrentField))
  where
    uFieldNames :: [Text] -> Int -> Level Interface -> Level Interface
    uFieldNames _  _ s@(WithDialog _ _) = s
    uFieldNames ns cf (Zoomed (NormalZoom zv) i) = Zoomed (NormalZoom (set zvTitle (ns !! cf) zv)) i
    uFieldNames ns _ (Back (AsTable tv)) = Back $ AsTable (set tvFieldNames ns tv)
    uFieldNames ns _ (Back (AsRows tv)) = Back $ AsRows (updateRvNames ns tv)

changeCurrentFieldType :: FieldType -> EventM Name State ()
changeCurrentFieldType nt = do
    s <- get
    ts <- uses sRowStore types
    when (ts  !! (s ^. sCurrentField) /= nt) $
        setSRowStore %= changeFieldType nt (fromIntegral $ s ^. sCurrentField)

newRow :: EventM Name State ()
newRow = do
    setSRowStore %= addEmptyRow
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
toggleZoom = do
    rst <- use sRowStore
    unless (nFields rst == 0) $
        use (sInterface . levelZoom) >>= \case
                Just _ -> sInterface . levelZoom .= Nothing
                Nothing -> modify zoom

zoomViewerFromState :: State -> ZoomViewer
zoomViewerFromState s = mkZoomViewer (currentFieldName s)
                                      (isFormulaCurrentField s)
                                      (currentField s)

zoom :: State -> State
zoom s = set (sInterface . levelZoom) (Just . NormalZoom $ zoomViewerFromState s) s

toggleProperties :: EventM Name State ()
toggleProperties = do
    rst <- use sRowStore
    unless (nFields rst == 0) $
        use (sInterface . fieldProperties) >>= \case
                    Just _ -> sInterface . fieldProperties .= Nothing
                    Nothing -> modify properties

properties :: State -> State
properties s = set (sInterface . fieldProperties) (Just $ mkFieldPropertiesDialog (currentFieldName s)
                                            (isFormulaCurrentField s)
                                            (currentField s)
                                            (currentFieldType s)
                                            (currentFieldFormula s)
                                            (s ^. sRowStore)
                                            (s ^. sIndex)
                                            ) s

activateSearch :: EventM Name State ()
activateSearch = do
   rst <- use sRowStore
   unless (nFields rst == 0) $ do
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
            pc <- liftIO $ siPathAndConf si
            let si' = case confPath pc of
                         Nothing -> let
                                       cnf = defaultConfFileName $ path pc
                                    in changePathAndConf (pc { confPath = Just cnf }) si
                         Just _ -> si
            liftIO $ writeRowStore si' sis rst
            setSRowStore %= setUnchanged

doBackup :: EventM Name State ()
doBackup = use sRowStore >>= (\case
    False -> return ()
    True -> do
        msi <- use sSourceInfo
        case msi of
            Nothing -> return ()
            Just (si, sis) -> do
                pc <- liftIO $ siPathAndConf si
                let conf = defaultBackupFileName <$> confPath pc
                    fp = defaultBackupFileName $ path pc
                    si' = changePathAndConf (PathAndConf fp True conf True) si
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
                pc <- liftIO $ siPathAndConf si
                let conf = defaultBackupFileName <$> confPath pc
                    fp = defaultBackupFileName $ path pc
                void . liftIO $ ((try $ do
                                    removeFile fp
                                    maybe (return ()) removeFile conf) :: IO (Either SomeException ())))
        . changed

