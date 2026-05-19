{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module TUI.YesNoDialog (
  YesNoDialog
  , mkYesNoDialog
  , renderYesNoDialog
  , handleEventYesNoDialog
  ) where


import Brick hiding (getName, zoom)
import Brick qualified as B
import Brick.Widgets.Dialog
import Control.Lens hiding (index, Zoom, zoom, Level, para)
import Data.Text (Text)
import Data.Text qualified as T
import Graphics.Vty.Input (Event(EvKey), Key (KEnter, KEsc), Button (BLeft))

import TUI.Base

data YesNoDialog = YesNoDialog { _ynMessage :: Text
                               , _ynDialog :: Dialog () Name }

makeLenses ''YesNoDialog

mkYesNoDialog :: Text -> Text -> YesNoDialog
mkYesNoDialog message ttle = YesNoDialog message
                                           (dialog (Just $ txt ttle)
                                           (Just (DButton OkButton, [ ("OK", DButton OkButton, ())
                                                                    , ("Cancel", DButton CancelButton, ())]))
                                           (T.length message + 4))

renderYesNoDialog :: YesNoDialog -> Widget Name
renderYesNoDialog ynd = renderDialog (ynd ^. ynDialog) $ txt " " <=> myTxt (" " <> ynd ^. ynMessage) <=> txt " "

handleEventYesNoDialog :: BrickEvent Name e -> EventM Name YesNoDialog (DialogEventResult ())
handleEventYesNoDialog (VtyEvent key@(EvKey k ms)) =
    case k of
        KEnter | null ms -> returnSelection
        KEsc | null ms -> return DialogCancel
        _ -> do
               B.zoom ynDialog $ handleDialogEvent key
               return DoNothing
handleEventYesNoDialog (MouseDown (DButton OkButton) BLeft [] _) = return $ DialogResult ()
handleEventYesNoDialog (MouseDown (DButton CancelButton) BLeft [] _) = return DialogCancel
handleEventYesNoDialog _ = return DoNothing

returnSelection :: EventM Name YesNoDialog (DialogEventResult ())
returnSelection = do
    sel <- uses ynDialog dialogSelection
    case sel of
        Just (DButton OkButton, _) -> return $ DialogResult ()
        Just (DButton CancelButton, _) -> return DialogCancel
        _ -> return DoNothing
