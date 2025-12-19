{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module TUI.Level (
  DialogLevel(..)
  , Interface
  , Level(..)
  , BackLevel(..)
  , ZoomLevel(..)
  , isDialog
  , isZoomed
  , isBack
  , renderDialogLevel
  , renderZoomLevel
  , renderBackLevel
  , updateLevels
  , getLevel
  , removeLevel
  , levelDialog
  , levelZoom
  , levelBack
  , quitDialog
  , searchDialog
  , fieldProperties
  , tableViewer
  , rowViewer
  , activeEditor

  , module TUI.FieldPropertiesDialog
  , module TUI.RowViewer
  , module TUI.SearchDialog
  , module TUI.TableViewer
  , module TUI.YesNoDialog
  , module TUI.ZoomViewer
  ) where


import Brick hiding (getName, zoom)
import Brick.Widgets.Border
import Brick.Widgets.Center
import Control.Lens hiding (index, Zoom, zoom, Level, para)
import Data.Text (Text)
import Model.Expression.RecursionSchemas ( Fix(..), bottomUp, cata, para, applyToFirst )

import TUI.Base
import TUI.FieldPropertiesDialog
import TUI.RowViewer
import TUI.SearchDialog
import TUI.TableViewer
import TUI.YesNoDialog
import TUI.ZoomViewer


data Level i = WithDialog DialogLevel i
             | Zoomed ZoomLevel i
             | Back BackLevel deriving Functor

data DialogLevel = Searching SearchDialog
                 | Quitting YesNoDialog
                 | FieldProperties FieldPropertiesDialog

newtype ZoomLevel = NormalZoom ZoomViewer

data BackLevel = AsTable TableViewer
               | AsRows RowViewer

isDialog :: Level i -> Bool
isDialog (WithDialog _ _) = True
isDialog _ = False

isZoomed :: Level i -> Bool
isZoomed (Zoomed _ _) = True
isZoomed _ = False

isBack :: Level i -> Bool
isBack (Back _) = True
isBack _ = False

type Interface = Fix Level

instance Show(Interface) where
    show = cata sl
        where sl (WithDialog _ x) = "WithDialog(" ++ x ++ ")"
              sl (Zoomed _ x) = "Zoomed(" ++ x ++ ")"
              sl (Back _) = "Back"

updateLevels :: (Level Interface -> Level Interface) -> Interface -> Interface
updateLevels = bottomUp

updateLevel :: (Level Interface -> Maybe (Level Interface)) -> Interface -> Interface
updateLevel = applyToFirst

instance HasEditor DialogLevel where
    editorLens = lens getter setter
        where getter (FieldProperties fp) = fp ^. editorLens
              getter _ = Nothing
              setter (FieldProperties fp) v = FieldProperties $ set editorLens v fp
              setter dl _ = dl

instance HasEditor ZoomLevel where
    editorLens = lens getter setter
        where getter (NormalZoom zv) = zv ^. editorLens
              setter (NormalZoom zv) v = NormalZoom $ set editorLens v zv

instance HasEditor BackLevel where
    editorLens = lens getter setter
        where getter (AsTable tv) = tv ^. editorLens
              getter (AsRows rv) = rv ^. editorLens
              setter (AsTable tv) v = AsTable $ set editorLens v tv
              setter (AsRows rv) v = AsRows $ set editorLens v rv

getLevel :: (forall i . Level i -> Bool) -> Interface -> Maybe (Level Interface)
getLevel f = para search
    where search :: Level (Interface, Maybe (Level Interface)) -> Maybe (Level Interface)
          search l@(WithDialog dl (i, ms)) = if f l then Just (WithDialog dl i) else ms
          search l@(Zoomed zl (i, _)) = if f l then Just (Zoomed zl i) else Nothing
          search l@(Back bl) = if f l then Just (Back bl) else Nothing

removeLevel :: (forall i . Level i -> Bool) -> Interface -> Interface
removeLevel f = updateLevel remL
    where remL l@(WithDialog _ i) = if f l then Just (out i) else Nothing
          remL l@(Zoomed _ i) = if f l then Just (out i) else Nothing
          remL l@(Back _) = if f l then error "Cannot remove back" else Nothing

levelDialog :: Lens' Interface (Maybe DialogLevel)
levelDialog = lens getter setter
    where getter i = do
                       WithDialog dl _ <- getLevel isDialog i
                       return dl

          setter i Nothing = removeLevel isDialog i
          setter i (Just dl) = updateLevel (addD dl) i
          addD dl (WithDialog _ i) = Just $ WithDialog dl i
          addD dl l = Just $ WithDialog dl (In l)

levelZoom :: Lens' Interface (Maybe ZoomLevel)
levelZoom = lens getter setter
    where getter i = do
                        Zoomed zl _ <- getLevel isZoomed i
                        return zl

          setter i Nothing = removeLevel isZoomed i
          setter i (Just z) = updateLevel (addZ z) i
          addZ _ (WithDialog _ _) = Nothing
          addZ z (Zoomed _ i) = Just $ Zoomed z i
          addZ z t@(Back _) = Just $ Zoomed z (In t)

levelBack :: Lens' Interface (Maybe BackLevel)
levelBack = lens getter setter
    where getter i = do
                       Back bl <- getLevel isBack i
                       return bl
          setter i Nothing = removeLevel isBack i
          setter i (Just bl) = updateLevel (addB bl) i
          addB bl (Back _) = Just $ Back bl
          addB _ _ = Nothing

searchDialog :: Lens' Interface (Maybe SearchDialog)
searchDialog = lens getter setter
    where getter i = do
                        Searching sd <- i ^. levelDialog
                        return sd
          setter i v = set levelDialog (fmap Searching v) i

quitDialog :: Lens' Interface (Maybe YesNoDialog)
quitDialog = lens getter setter
    where getter i = do
                        Quitting ynd <- i ^. levelDialog
                        return ynd
          setter i v = set levelDialog (fmap Quitting v) i

fieldProperties :: Lens' Interface (Maybe FieldPropertiesDialog)
fieldProperties = lens getter setter
    where getter i = do
                        FieldProperties rz <- i ^. levelDialog
                        return rz

          setter i v = set levelDialog (fmap FieldProperties v) i

tableViewer :: Lens' Interface (Maybe TableViewer)
tableViewer = lens getter setter
    where getter i = do
                        AsTable tv <- i ^. levelBack
                        return tv
          setter i v = set levelBack (fmap AsTable v) i

rowViewer :: Lens' Interface (Maybe RowViewer)
rowViewer = lens getter setter
    where getter i = do
                        AsRows rv <- i ^. levelBack
                        return rv
          setter i v = set levelBack (fmap AsRows v) i

activeEditor :: Lens' Interface (Maybe ValueEditor)
activeEditor = lens getter setter
    where getter (In (WithDialog dl _)) = dl ^. editorLens
          getter (In (Zoomed zl _)) = zl ^. editorLens
          getter (In (Back bl)) = bl ^. editorLens

          setter _ Nothing = error "Cannot remove editor"
          setter (In (WithDialog dl i)) (Just ve) = In (WithDialog (set editorLens (Just ve) dl) i)
          setter (In (Zoomed zl i)) (Just ve) = In (Zoomed (set editorLens (Just ve) zl) i)
          setter (In (Back bl)) (Just ve) = In (Back (set editorLens (Just ve) bl))


renderDialogLevel :: DialogLevel -> Widget Name
renderDialogLevel (Searching sd) = renderSearchDialog sd
renderDialogLevel (Quitting ynd) = renderYesNoDialog ynd
renderDialogLevel (FieldProperties fp) = renderFieldPropertiesDialog fp

renderZoomLevel :: ZoomLevel -> Widget Name
renderZoomLevel (NormalZoom zv) = renderZoomViewer zv

renderBackLevel :: Text -> BackLevel -> Widget Name
renderBackLevel title (AsTable tv) = renderBack title (renderTableViewer tv) tableHelp
  where tableHelp = "C-z: zoom, C-r: field pRoperties, C-t: return to field view, C-f: find, C-w: write, C-q: exit"
renderBackLevel title (AsRows rv) = renderBack title (renderRowViewer rv) rowHelp
  where rowHelp = "C-z: zoom, C-r: field pRoperties, C-t: table view, C-f: find, C-n: new row, C-w: write, C-q: exit"

renderBack :: Text -> Widget Name -> Text -> Widget Name
renderBack t content help = joinBorders $ center $
       borderWithLabel (withAttr titleAttr . txt $ t) $
       content
       <=>
       hBorder
       <=>
       hCenter (txt help)

