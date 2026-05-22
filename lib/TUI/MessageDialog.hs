{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module TUI.MessageDialog (
  MessageDialog
  , mkMessageDialog
  , renderMessageDialog
  , handleEventMessageDialog
  ) where


import Brick hiding (getName, zoom)
import Brick qualified as B
import Brick.Widgets.Dialog
import Control.Lens hiding (index, Zoom, zoom, Level, para)
import Data.Text (Text)
import Data.Text qualified as T
import Graphics.Vty.Input (Event(EvKey), Key (KEnter, KEsc), Button (BLeft))

import TUI.Base

data MessageDialog = MessageDialog { _msgMessage :: Text
                                   , _msgDialog :: Dialog () Name }

makeLenses ''MessageDialog

mkMessageDialog :: Text -> Text -> Bool -> MessageDialog
mkMessageDialog message ttle cancel = MessageDialog message
                                           (dialog (Just $ txt ttle)
                                           (Just (DButton OkButton, buttons))
                                           (T.length message + 4))
    where buttons = if cancel
                    then [ ("OK", DButton OkButton, ())
                         , ("Cancel", DButton CancelButton, ())]
                    else [ ("OK", DButton OkButton, ()) ]

renderMessageDialog :: MessageDialog -> Widget Name
renderMessageDialog msgd = renderDialog (msgd ^. msgDialog) $ txt " " <=> myTxt (" " <> msgd ^. msgMessage) <=> txt " "

handleEventMessageDialog :: BrickEvent Name e -> EventM Name MessageDialog (DialogEventResult ())
handleEventMessageDialog (VtyEvent key@(EvKey k ms)) =
    case k of
        KEnter | null ms -> returnSelection
        KEsc | null ms -> return DialogCancel
        _ -> do
               B.zoom msgDialog $ handleDialogEvent key
               return DoNothing
handleEventMessageDialog (MouseDown (DButton OkButton) BLeft [] _) = return $ DialogResult ()
handleEventMessageDialog (MouseDown (DButton CancelButton) BLeft [] _) = return DialogCancel
handleEventMessageDialog _ = return DoNothing

returnSelection :: EventM Name MessageDialog (DialogEventResult ())
returnSelection = do
    sel <- uses msgDialog dialogSelection
    case sel of
        Just (DButton OkButton, _) -> return $ DialogResult ()
        Just (DButton CancelButton, _) -> return DialogCancel
        _ -> return DoNothing
