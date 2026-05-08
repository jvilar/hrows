{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module TUI.SearchDialog (
  SearchDialog
  , sdValues
  , sdDialog
  , mkSearchDialog
  , emptySearchDialog
  , renderSearchDialog
  , handleEventSearchDialog
  ) where



import Brick hiding (getName, zoom)
import Brick qualified as B
import Brick.Widgets.Dialog
import Brick.Widgets.List hiding (splitAt, reverse)
import Control.Lens hiding (index, Level, para)
import Data.Char (isLower, isAlphaNum)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Graphics.Vty.Input.Events(Event(EvKey), Key(..), Button (..))

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

emptySearchDialog :: SearchDialog
emptySearchDialog = SearchDialog (list SearchList V.empty 1) (dialog Nothing Nothing 0)

renderSearchDialog :: SearchDialog -> Widget Name
renderSearchDialog sd = Widget Fixed Greedy $ do
    h <- availHeight <$> getContext
    let l = min (h - 4) (1 + V.length (sd ^. sdValues . listElementsL))
    render $ renderDialog (sd ^. sdDialog) $ vLimit l (renderList renderValue False (sd ^. sdValues) <=> txt " ")

listKeys :: [Key]
listKeys = [KDown, KUp, KPageUp, KPageDown, KHome, KEnd, KLeft, KRight]


handleEventSearchDialog :: BrickEvent Name e
                        -> EventM Name SearchDialog (DialogEventResult Text)
handleEventSearchDialog (VtyEvent key@(EvKey k ms)) =
    case k of
         KEnter | null ms -> returnSelection
         KEsc | null ms -> return DialogCancel
         KChar c | null ms && isAlphaNum c -> do
                    vs <- use $ sdValues . listElementsL
                    let mi = case V.findIndex (T.isPrefixOf (T.singleton c)) vs of
                               Nothing -> if isLower c
                                          then V.findIndex (T.isPrefixOf (T.toUpper (T.singleton c))) vs
                                          else V.findIndex (T.isPrefixOf (T.toLower (T.singleton c))) vs
                               Just i -> Just i
                    case mi of
                        Nothing -> return DoNothing
                        Just i -> do
                                     sdValues %= listMoveTo i
                                     return DoNothing

         _ | null ms && k `elem` listKeys -> do
                  B.zoom sdValues $ handleListEvent key
                  return DoNothing
           | otherwise -> do
                  B.zoom sdDialog $ handleDialogEvent key
                  return DoNothing
handleEventSearchDialog (MouseDown SearchList BLeft [] (Location (_, r))) = do
        vs <- use $ sdValues . listElementsL
        if r < 0 || r >= V.length vs
            then return DoNothing
            else returnSelection
handleEventSearchDialog (MouseDown SearchList BScrollDown [] _) = do
    B.zoom sdValues (handleListEvent (EvKey KPageDown []))
    return DoNothing
handleEventSearchDialog (MouseDown SearchList BScrollUp [] _) = do
    B.zoom sdValues (handleListEvent (EvKey KPageUp []))
    return DoNothing
handleEventSearchDialog (MouseDown (DButton OkButton) BLeft [] _) = returnSelection
handleEventSearchDialog (MouseDown (DButton CancelButton) BLeft [] _) = return DialogCancel
handleEventSearchDialog _ = return DoNothing


returnSelection :: EventM Name SearchDialog (DialogEventResult Text)
returnSelection = do
    sel <- uses sdDialog dialogSelection
    case sel of
        Nothing -> return DoNothing
        Just (DButton CancelButton, _) -> return DialogCancel
        Just (DButton OkButton, _) -> do
            selVal <- uses sdValues listSelectedElement
            return $ case selVal of
                Nothing -> DoNothing
                Just (_, t) -> DialogResult t
        _ -> return DoNothing
