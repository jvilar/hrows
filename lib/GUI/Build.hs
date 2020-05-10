{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module GUI.Build (
            -- *Types
            GUIControl
            -- *Functions
            , makeGUI
) where

import Control.Concurrent.Chan(Chan)
import Data.Maybe(fromJust)
import Data.Text(Text)
import qualified Data.Text as T

import GI.Gdk (screenGetDefault)
import GI.Gtk hiding (MessageDialog)

import Paths_hrows(getDataFileName)

import GUI.BuildMonad
import GUI.CanBeCast
import GUI.Control
import GUI.DialogManager.Build
import GUI.Iteration
import GUI.ListingWindow.Build
import GUI.MainWindow.Build
import GUI.HKD
import Presenter.Input

makeGUI :: Chan Input -> IO GUIControl
makeGUI iChan = do
  _ <- GI.Gtk.init Nothing

  builder <- builderNew
  gladefn <- getDataFileName "src/hrows.glade"
  _ <- builderAddFromFile builder $ T.pack gladefn

  styleFile <- getDataFileName "src/hrows.css"

  provider <- cssProviderNew
  cssProviderLoadFromPath provider $ T.pack styleFile
  Just screen <- screenGetDefault
  styleContextAddProviderForScreen screen provider $ fromIntegral STYLE_PROVIDER_PRIORITY_APPLICATION

  control <- prepareControl iChan builder
  runBuild  builder control $ do
    configureMainWindow
    configureListingWindow $ listingWindow control
    configureDialogManager
  return control


prepareControl :: Chan Input -> Builder -> IO GUIControl
prepareControl iChan builder = do
  let getObject :: CanBeCast obj => Text -> IO obj
      getObject name = builderGetObject builder name >>= doCast . fromJust
  fromIO GUIControl {
    mainWindow = buildMainWindow iChan builder
    , listingWindow = buildListingWindow iChan builder
    , inputChan = return iChan
    , dialogManager = buildDialogManager builder
    }


notImplementedDialog :: Text -> Input
notImplementedDialog f = toInput $ MessageDialog (InformationMessage  $ T.concat ["Opción ", f, " no implementada"])
