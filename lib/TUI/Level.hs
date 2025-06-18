{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

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
  , searchDialog
  , richZoom
  , tableViewer
  , rowViewer
  , activeEditor

  , module TUI.RichZoomViewer
  , module TUI.RowViewer
  , module TUI.SearchDialog
  , module TUI.TableViewer
  , module TUI.ZoomViewer
  ) where


import Brick hiding (getName, zoom)
import Brick.Widgets.Border
import Brick.Widgets.Center
import Control.Lens hiding (index, Zoom, zoom, Level, para)
import Data.Text (Text)
import Model.Expression.RecursionSchemas ( Fix(..), bottomUp, cata, para )

import TUI.Base
import TUI.RichZoomViewer
import TUI.RowViewer
import TUI.SearchDialog
import TUI.TableViewer
import TUI.ZoomViewer


data Level i = WithDialog DialogLevel i
             | Zoomed ZoomLevel i
             | Back BackLevel deriving Functor

newtype DialogLevel = Searching SearchDialog

data ZoomLevel = NormalZoom ZoomViewer
               | RichZoom RichZoomViewer

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

updateLevels :: (Level Interface -> Level Interface) -> Interface -> Interface
updateLevels = bottomUp

instance HasEditor DialogLevel where
    editorLens = lens getter setter
        where getter _ = Nothing
              setter dl _ = dl

instance HasEditor ZoomLevel where
    editorLens = lens getter setter
        where getter (NormalZoom zv) = zv ^. editorLens
              getter (RichZoom rz) = rz ^. editorLens
              setter (NormalZoom zv) v = NormalZoom $ set editorLens v zv
              setter (RichZoom rz) v = RichZoom $ set editorLens v rz

instance HasEditor BackLevel where
    editorLens = lens getter setter
        where getter (AsTable tv) = tv ^. editorLens
              getter (AsRows rv) = rv ^. editorLens
              setter (AsTable tv) v = AsTable $ set editorLens v tv
              setter (AsRows rv) v = AsRows $ set editorLens v rv

getLevel :: (forall i . Level i -> Bool) -> Interface -> Maybe Interface
getLevel f = para search
    where search :: Level (Interface, Maybe Interface) -> Maybe Interface
          search l@(WithDialog dl (i, ms)) = if f l then Just (In $ WithDialog dl i) else ms
          search l@(Zoomed zl (i, _)) = if f l then Just (In $ Zoomed zl i) else Nothing
          search l@(Back bl) = if f l then Just (In $ Back bl) else Nothing

removeLevel :: (forall i . Level i -> Bool) -> Interface -> Interface
removeLevel f = updateLevels remL
    where remL l@(WithDialog _ i) = if f l then out i else l
          remL l@(Zoomed _ i) = if f l then out i else l
          remL l@(Back _) = if f l then error "Cannot remove back" else l

levelDialog :: Lens' Interface (Maybe DialogLevel)
levelDialog = lens getter setter
    where getter i = case getLevel isDialog i of
                         Just (In (WithDialog dl _)) -> Just dl
                         _ -> Nothing

          setter i Nothing = removeLevel isDialog i
          setter i (Just dl) = updateLevels (addD dl) i
          addD :: DialogLevel -> Level Interface -> Level Interface
          addD _ (WithDialog _ i) = out i
          addD dl (Zoomed zl (In (WithDialog _ i))) = WithDialog dl (In $ Zoomed zl i)
          addD _ z@(Zoomed _ _) = z
          addD dl t@(Back _) = WithDialog dl (In t)

levelZoom :: Lens' Interface (Maybe ZoomLevel)
levelZoom = lens getter setter
    where getter i = case getLevel isZoomed i of
                         Just (In (Zoomed zl _)) -> Just zl
                         _ -> Nothing

          setter i Nothing = removeLevel isZoomed i
          setter i (Just z) = updateLevels (addZ z) i
          addZ :: ZoomLevel -> Level Interface -> Level Interface
          addZ _ s@(WithDialog _ _) = s
          addZ _ (Zoomed _ i) = out i
          addZ z t@(Back _) = Zoomed z (In t)

levelBack :: Lens' Interface (Maybe BackLevel)
levelBack = lens getter setter
    where getter i = do
                       In (Back bl) <- getLevel isBack i
                       return bl
          setter i Nothing = removeLevel isBack i
          setter i (Just bl) = updateLevels (addB bl) i
          addB :: BackLevel -> Level Interface -> Level Interface
          addB _ s@(WithDialog _ _) = s
          addB _ z@(Zoomed _ _) = z
          addB bl (Back _) = Back bl

searchDialog :: Lens' Interface (Maybe SearchDialog)
searchDialog = lens getter setter
    where getter i = do
                        Searching sd <- i ^. levelDialog
                        return sd
          setter i v = set levelDialog (fmap Searching v) i

richZoom :: Lens' Interface (Maybe RichZoomViewer)
richZoom = lens getter setter
    where getter i = do
                        RichZoom rz <- i ^. levelZoom
                        return rz

          setter i v = set levelZoom (fmap RichZoom v) i

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
    where getter = cata ae
          ae (WithDialog dl _) = dl ^. editorLens
          ae (Zoomed zl _) = zl ^. editorLens
          ae (Back bl) = bl ^. editorLens

          setter _ Nothing = error "Cannot remove editor"
          setter i (Just ve) = para (addE ve) i
          addE :: ValueEditor -> Level (Interface, Interface) -> Interface
          addE _ (WithDialog dl (_, i)) = In $ WithDialog dl i
          addE ve (Zoomed zl (i, _)) = In $ Zoomed (set editorLens (Just ve) zl) i
          addE ve (Back bl) = In $ Back $ set editorLens (Just ve) bl


renderDialogLevel :: DialogLevel -> Widget Name
renderDialogLevel (Searching sd) = renderSearchDialog sd

renderZoomLevel :: ZoomLevel -> Widget Name
renderZoomLevel (NormalZoom zv) = renderZoomViewer zv
renderZoomLevel (RichZoom rz) = renderRichZoomViewer rz

renderBackLevel :: Text -> BackLevel -> Widget Name
renderBackLevel title (AsTable tv) = renderBack title (renderTableViewer tv) tableHelp
  where tableHelp = "C-z: zoom, C-r: rich zoom, C-t: return to field view, C-f: find, C-w: write, C-q: exit"
renderBackLevel title (AsRows rv) = renderBack title (renderRowViewer rv) rowHelp
  where rowHelp = "C-z: zoom, C-r: rich zoom, C-t: table view, C-f: find, C-n: new row, C-w: write, C-q: exit"

renderBack :: Text -> Widget Name -> Text -> Widget Name
renderBack t content help = joinBorders $ center $
       borderWithLabel (withAttr titleAttr . txt $ t) $
       content
       <=>
       hBorder
       <=>
       hCenter (txt help)

