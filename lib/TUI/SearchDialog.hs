{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module TUI.SearchDialog (
  SearchDialog
  , sdValues
  , sdDialog
  , mkSearchDialog
  , renderSearchDialog
  ) where


import Brick hiding (getName, zoom)
import Brick.Widgets.Dialog
import Brick.Widgets.List hiding (splitAt, reverse)
import Control.Lens hiding (index, Zoom, zoom, Level, para)
import Data.Text (Text)
import Data.Vector qualified as V

import TUI.Base

data SearchDialog = SearchDialog { _sdValues :: List Name Text
                                 , _sdDialog :: Dialog () Name
                                 }

makeLenses ''SearchDialog

mkSearchDialog :: Name -> Int -> Text -> [Text] -> SearchDialog
mkSearchDialog n w ttle values = SearchDialog (list n (V.fromList values) 1)
                                            (dialog (Just $ txt ttle)
                                                    (Just (DButton OkButton, [ ("OK", DButton OkButton, ())
                                                              , ("Cancel", DButton CancelButton, ())]))
                                                    w
                                            )

renderSearchDialog :: SearchDialog -> Widget Name
renderSearchDialog sd = Widget Fixed Greedy $ do
    h <- availHeight <$> getContext
    let l = min (h - 4) (1 + V.length (sd ^. sdValues . listElementsL))
    render $ renderDialog (sd ^. sdDialog) $ vLimit l (renderList renderValue False (sd ^. sdValues) <=> txt " ")
