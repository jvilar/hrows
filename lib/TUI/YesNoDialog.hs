{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module TUI.YesNoDialog (
  YesNoDialog
  , ynDialog
  , mkYesNoDialog
  , renderYesNoDialog
  ) where


import Brick hiding (getName, zoom)
import Brick.Widgets.Dialog
import Control.Lens hiding (index, Zoom, zoom, Level, para)
import Data.Text (Text)
import Data.Text qualified as T

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
