{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module GUI.ListingWindow.Build (
    -- *Functions
    buildListingWindow
    , configureListingWindow
) where

import Control.Concurrent.Chan (Chan)
import Control.Monad(void, (>=>))
import Control.Monad.IO.Class(liftIO)
import Data.IntMap qualified as IM
import Data.Maybe(fromJust)
import Data.Text (Text)
import GI.Gtk
import GI.Gdk(keyvalName, EventKey, ModifierType(..))

import Presenter.Input

import GUI.Control
import GUI.CanBeCast
import GUI.BuildMonad
import GUI.ListingWindow
import GUI.HKD (fromIO)


buildListingWindow :: Chan Input -> Builder -> IO ListingWindow
buildListingWindow iChan builder = do
  let
    getObject :: CanBeCast obj => Text -> IO obj
    getObject name = builderGetObject builder name >>= doCast . fromJust
  fromIO ListingWindow {
    windowLW = getObject "listingWindow"
    , rendererLW = cellRendererTextNew
    , viewLW = getObject "listingView"
    , filterEntryLW = getObject "listingFilterEntry"
    , filteredPosLW = return IM.empty
    , inputChanLW = return iChan
  }

configureListingWindow :: ListingWindow -> BuildMonad ()
configureListingWindow w = do
                prepareListingWindow w
                prepareFilterEntry w
                prepareCursorBindings w

globalKeys :: [ ((Text, [ModifierType]), Input)]
globalKeys = [ (("Page_Down", []), toInput MoveNext)
             , (("KP_Next", []), toInput MoveNext)
             , (("Page_Up", []), toInput MovePrevious)
             , (("KP_Page_Up", []), toInput MovePrevious)
             , (("q", [ModifierTypeControlMask]), toInput ExitRequested)
             , (("r", [ModifierTypeControlMask]), toInput Redo)
             , (("z", [ModifierTypeControlMask]), toInput Undo)
             , (("Return", []), toInput DoNothing)
             ]

ignoredModfifiers :: [ModifierType]
ignoredModfifiers = [ModifierTypeLockMask, ModifierTypeMod2Mask]

commandFromGlobalKey :: EventKey -> IO (Maybe Input)
commandFromGlobalKey evk = do
  n <- get evk #keyval >>= keyvalName
  case n of
    Just name -> do
      mods <- filter (`notElem` ignoredModfifiers) <$> get evk #state
      return $ lookup (name, mods) globalKeys
    Nothing -> return Nothing


prepareListingWindow :: ListingWindow -> BuildMonad()
prepareListingWindow lw = do
  control <- getControl
  let w = windowLW lw
  _ <- liftIO $ do
    _ <- w `on` #deleteEvent $ const $ do
      liftIO $ sendInput control CloseListingRequested
      return True
    w `on` #keyPressEvent $
      (commandFromGlobalKey >=>
       (\case
          Nothing -> return False
          Just cmd -> do
            liftIO $ sendInput control cmd
            return True))
  return ()

prepareFilterEntry :: ListingWindow -> BuildMonad ()
prepareFilterEntry w = do
  control <- getControl
  let entry = filterEntryLW w
  entry `set` [ #sensitive := True ]
  _ <- entry `on` #keyReleaseEvent $ \evk -> do
    buffer <- textViewGetBuffer entry
    begin <- #getStartIter buffer
    end <- #getEndIter buffer
    f <- #getText buffer begin end False
    liftIO $ sendInput control (ChangeFilter f)
    return True
  return ()

prepareCursorBindings :: ListingWindow -> BuildMonad ()
prepareCursorBindings w = do
  let tv = viewLW w
  control <- getControl
  void $ after tv #cursorChanged $
    getCurrentRow w >>= \case
      Nothing -> return ()
      Just r -> liftIO . sendInput control $ MoveHere r
