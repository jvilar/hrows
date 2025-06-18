{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module TUI.Level (
  DialogLevel(..)
  , Interface
  , Level(..)
  , BackLevel(..)
  , ZoomLevel(..)
  , ZoomViewer(..)
  , zvValue
  , RichZoomViewer(..)
  , RichZoomFocus(..)
  , ivFocus
  , ivValue
  , ivIsFormula
  , SearchDialog(..)
  , sdValues
  , sdDialog
  , isDialog
  , isZoomed
  , isBack
  , RowViewer(..)
  , rvFieldNames
  , rvValueList
  , valueList
  , TableViewer(..)
  , tvLists
  , tvCurrentField
  , mkSearchDialog
  , renderSearchDialog
  , renderZoomViewer
  , renderRichZoomViewer
  , mkRowViewer
  , mkZoomViewer
  , mkRichZoomViewer
  , updateZoomViewer
  , updateRichZoomViewer
  , richZoomMoveUp
  , richZoomMoveDown
  , richZoomNextType
  , richZoomPrevType
  , buildTable
  , renderDialogLevel
  , renderZoomLevel
  , renderBackLevel
  , updateLevels
  , updateRvValues
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
  ) where


import Brick hiding (getName, zoom)
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Dialog
import Brick.Widgets.Edit qualified as Ed
import Brick.Widgets.List hiding (splitAt, reverse)
import Control.Lens hiding (index, Zoom, zoom, Level, para)
import Data.List(transpose, intersperse)
import Data.Maybe(fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Zipper qualified as Tz
import Data.Vector qualified as V
import Model.Expression.RecursionSchemas ( Fix(..), bottomUp, cata, para )
import Model.RowStore
import TUI.Base


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

data SearchDialog = SearchDialog { _sdValues :: List Name Text
                                 , _sdDialog :: Dialog () Name
                                 }

data RowViewer = RowViewer { _rvFieldNames :: List Name Text
                           , _rvFieldWidth :: Int
                           , _rvValueList :: List Name ValueViewer
                           }

data TableViewer = TableViewer { _tvFieldNames :: [Text]
                               , _tvColWidths :: [Int]
                               , _tvColumns :: [List Name Text]
                               , _tvCurrentField :: Int
                               }

data ZoomViewer = ZoomViewer { _zvTitle :: Text
                             , _zvValue :: ValueViewer
                             }

data RichZoomFocus = RzValue | RzType | RzFMark | RzFormula deriving (Eq, Ord, Enum)

data RichZoomViewer = RichZoomViewer { _ivTitle :: Text
                                     , _ivValue :: ValueViewer
                                     , _ivType :: Text
                                     , _ivIsFormula :: Bool
                                     , _ivFormula :: ValueEditor
                                     , _ivFocus :: RichZoomFocus
                                     }

tvLists :: Traversal' TableViewer (List Name Text)
tvLists f (TableViewer fl cw cs cf) = TableViewer fl cw <$> traverse f cs <*> pure cf

makeLenses ''RowViewer
makeLenses ''SearchDialog
makeLenses ''TableViewer
makeLenses ''ZoomViewer
makeLenses ''RichZoomViewer

instance HasEditor DialogLevel where
    editorLens = lens getter setter
        where getter _ = Nothing
              setter dl _ = dl

instance HasEditor ZoomViewer where
    editorLens = lens getter setter
        where getter zv = case zv ^. zvValue of
                             Left ve -> Just ve
                             _ -> Nothing
              setter _ Nothing = error "Cannot remove editor from zoom viewer"
              setter zv (Just ve) = set zvValue (Left ve) zv

instance HasEditor RichZoomViewer where
    editorLens = lens getter setter
        where getter iv = case iv ^. ivFocus of
                              RzValue -> case iv ^. ivValue of
                                             Left ve -> Just ve
                                             _ -> Nothing
                              RzFormula -> if iv ^. ivIsFormula
                                             then Just $ iv ^. ivFormula
                                             else Nothing
                              _ -> Nothing
              setter _ Nothing = error "Cannot remove editor from rich zoom viewer"
              setter iv (Just ve) = case iv ^. ivFocus of
                                      RzValue -> set ivValue (Left ve) iv
                                      RzFormula -> set ivFormula ve iv
                                      _ -> error "Bad focus in rich zoom for setting editor"

instance HasEditor ZoomLevel where
    editorLens = lens getter setter
        where getter (NormalZoom zv) = zv ^. editorLens
              getter (RichZoom iv) = iv ^. editorLens
              setter (NormalZoom zv) v = NormalZoom $ set editorLens v zv
              setter (RichZoom zv) v = RichZoom $ set editorLens v zv


instance HasEditor RowViewer where
    editorLens = lens getter setter
        where getter rv = do
                             (_, Left ve) <- listSelectedElement $ rv ^. rvValueList
                             return ve
              setter _ Nothing = error "Cannot remove editor from row viewer"
              setter rv (Just ve) = set (rvValueList . element (fromMaybe 0 $ listSelected $ rv ^. rvValueList)) (Left ve) rv

instance HasEditor TableViewer where
    editorLens = lens (const Nothing) setter
        where setter _ Nothing = error "Cannot remove editor from table viewer"
              setter _ _ = error "Cannot set editor in table viewer"

instance HasEditor BackLevel where
    editorLens = lens getter setter
        where getter (AsTable tv) = tv ^. editorLens
              getter (AsRows rv) = rv ^. editorLens
              setter (AsTable tv) v = AsTable $ set editorLens v tv
              setter (AsRows rv) v = AsRows $ set editorLens v rv

updateRvValues :: [Field] -> RowViewer -> RowViewer
updateRvValues ts rv = over rvValueList (listReplace v i) rv
  where i = listSelected $ rv ^. rvValueList
        l = listElements (rv ^. rvValueList)
        v = V.fromList $ zipWith updateValueViewer ts (V.toList l)

updateValueViewer :: Field -> ValueViewer -> ValueViewer
updateValueViewer f = either (Left . updateEditor f) (const $ Right f)

updateEditor :: Field -> ValueEditor -> ValueEditor
updateEditor f ve | t == T.concat (Ed.getEditContents $ ve ^. veEditor) = set veIsError (isError f) ve
                  | otherwise = over veEditor (Ed.applyEdit (const $ Tz.textZipper [t] $ Just 1))
                                $ set veIsError (isError f) ve
                  where t = toString f

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
                        RichZoom iv <- i ^. levelZoom
                        return iv

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


mkSearchDialog :: Name -> Int -> Text -> [Text] -> SearchDialog
mkSearchDialog n w ttle values = SearchDialog (list n (V.fromList values) 1)
                                            (dialog (Just $ txt ttle)
                                                    (Just (DButton OkButton, [ ("OK", DButton OkButton, ())
                                                              , ("Cancel", DButton CancelButton, ())]))
                                                    w
                                            )


renderSearchDialog :: SearchDialog -> Widget Name
renderSearchDialog sd = renderDialog (sd ^. sdDialog) $
                           vLimit (V.length $ listElements $ sd ^. sdValues)
                                  (renderList renderValue False $ sd ^. sdValues)

renderZoomViewer :: ZoomViewer -> Widget Name
renderZoomViewer zv = centerLayer $ joinBorders $
                       hLimitPercent 95 $
                       borderWithLabel (withAttr titleAttr $ txt $ zv ^. zvTitle) $
                         renderValueViewer (zv ^. zvValue)
                         <=>
                         hBorder
                         <=>
                         hCenter (txt "C-r: rich zoom, C-z: close zoom")

renderRichZoomViewer :: RichZoomViewer -> Widget Name
renderRichZoomViewer iv = centerLayer $ joinBorders $
                       hLimitPercent 95 $
                       borderWithLabel (withAttr titleAttr $ txt $ iv ^. ivTitle) $
                         renderValueViewer (iv ^. ivValue)
                         <=>
                         hBorder
                         <=>
                          wFocus RzType (txt $ iv ^. ivType)
                         <=>
                         hBorder
                         <=>
                         if iv ^. ivIsFormula
                         then wFocus RzFMark (txt "[X]") <+> txt " " <+> renderValueEditor (iv ^. ivFormula)
                         else wFocus RzFMark (txt "[ ]")
                         <=>
                         hBorder
                         <=>
                         hCenter (txt "C-r: close rich zoom, C-z: zoom")
            where wFocus n = if iv ^. ivFocus == n
                      then withAttr selectedElementAttr
                      else id


renderValueViewer :: ValueViewer -> Widget Name
renderValueViewer = either renderValueEditor renderField
  where renderField f = let
              attr = if isError f then errorAttr else formulaAttr
           in withAttr attr . myTxt $ toString f


maxWidth :: Int
maxWidth = 40

mkRowViewer :: RowStore -> Int -> RowViewer
mkRowViewer rst pos = RowViewer { _rvFieldNames = listMoveTo pos fl
                                , _rvFieldWidth = min maxWidth (V.maximum . V.map T.length $ listElements fl)
                                , _rvValueList = valueList pos rst
                                }
                            where fl = fieldList rst

mkZoomViewer :: Text -> Bool -> Field -> ZoomViewer
mkZoomViewer fname isFormula field = ZoomViewer fname $ if isFormula
                                                        then Right field
                                                        else Left (mkEditor ZoomEditor field)

mkRichZoomViewer :: Text -> Bool -> Field -> Text -> Maybe Formula -> RichZoomViewer
mkRichZoomViewer fname isFormula field fType mFormula = RichZoomViewer fname
                            (if isFormula
                             then Right field
                             else Left (mkEditor RichZoomValueEditor field)
                             )
                            fType
                            (isJust mFormula)
                            (mkEditor RichZoomFormulaEditor $ toField $ fromMaybe "" mFormula)
                            RzValue

updateZoomViewer :: Field -> ZoomViewer -> ZoomViewer
updateZoomViewer f = over zvValue (either (Left . updateEditor f) (Right . const f))

updateRichZoomViewer :: Field -> RichZoomViewer -> RichZoomViewer
updateRichZoomViewer f = over ivValue (either (Left . updateEditor f) (Right . const f))

richZoomMoveUp :: RichZoomViewer -> RichZoomViewer
richZoomMoveUp = over ivFocus (\i -> if i > RzValue then pred i else RzValue)

richZoomMoveDown :: RichZoomViewer -> RichZoomViewer
richZoomMoveDown rz = over ivFocus (\i -> if i < limit then succ i else limit) rz
  where
    limit = if rz ^. ivIsFormula then RzFormula else RzFMark

richZoomNextType :: RichZoomViewer -> Maybe (RichZoomViewer, FieldType)
richZoomNextType rz
   | rz ^. ivFocus /= RzType || t == maxBound || t' == TypeEmpty = Nothing
   | otherwise = Just (set ivType (T.pack $ show t') rz, t')
   where t = read . T.unpack $ rz ^. ivType
         t' = succ t

richZoomPrevType :: RichZoomViewer -> Maybe (RichZoomViewer, FieldType)
richZoomPrevType rz
   | rz ^. ivFocus /= RzType || t == minBound = Nothing
   | otherwise = Just (set ivType (T.pack $ show t') rz, t')
   where t = read . T.unpack $ rz ^. ivType
         t' = pred t

mkEditor :: Name -> Field -> ValueEditor
mkEditor n f = ValueEditor (Ed.editor n (Just 1) $ toString f) (isError f)

fieldList :: RowStore -> List Name Text
fieldList rst = list FieldNames (V.fromList $ fnames rst) 1

valueList :: Int -> RowStore -> List Name ValueViewer
valueList pos rst = list ValueList (V.fromList $ zipWith valueWidget [0..] (row pos rst)) 1
    where valueWidget n f
              | isFormula n rst = Right f
              | otherwise = Left $ mkEditor (ValueViewer $ fromIntegral n) f

buildTable :: RowStore -> Int -> TableViewer
buildTable rst = TableViewer ns ws cls
    where cs = transpose $ map (map toString) $ rows rst
          cls = zipWith f [0..] cs
          f n r = list (ValueColumn n) (V.fromList r) 1
          ns = fnames rst
          ws = zipWith max (map T.length ns)
                           $ map (maximum . map T.length) cs

renderDialogLevel :: DialogLevel -> Widget Name
renderDialogLevel (Searching sd) = renderSearchDialog sd

renderZoomLevel :: ZoomLevel -> Widget Name
renderZoomLevel (NormalZoom zv) = renderZoomViewer zv
renderZoomLevel (RichZoom iv) = renderRichZoomViewer iv

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


renderRowViewer :: RowViewer -> Widget Name
renderRowViewer rv = Widget Greedy Fixed $ do
    h <- availHeight <$> getContext
    let v = min (V.length $ listElements $ rv ^. rvFieldNames) (h-2)
    render $ vLimit v $ hBox [
                hLimit (rv ^. rvFieldWidth) (renderList renderName False $ rv ^. rvFieldNames)
                , vBorder
                , renderList (const renderValueViewer) False
                      $ rv ^. rvValueList
              ]

renderTableViewer :: TableViewer -> Widget Name
renderTableViewer tv = Widget Greedy Fixed $ do
    h <- availHeight <$> getContext
    w <- availWidth <$> getContext
    let v = min (V.length $ listElements $ head $ tv ^. tvColumns) (h-4)
        ws = allocateWidths (tv ^. tvCurrentField) w $ tv ^. tvColWidths
    render ( vLimit 1 (hBox $ withWidths (\(i, t) ->
                                              if i == tv ^. tvCurrentField
                                              then withAttr selectedElementAttr $ myTxt t
                                              else withAttr titleAttr $ myTxt t)
                               ws
                               (zip [0..] $ tv ^. tvFieldNames)
                      )
             <=>
             hBorder
             <=>
             vLimit v ( hBox $ withWidths (renderList renderValue False)
                               ws
                               (tv ^. tvColumns)
                      )
           )


allocateWidths :: Int -> Int -> [Int] -> [Int]
allocateWidths curF w ws
  | w < s = let
               (lft, wcurF:rgt) = splitAt curF ws1
               wCurrent = min w (wcurF - 1)
               wsLeft= reverse . upto (w - wCurrent) $ reverse lft
               wRest = w - wCurrent - sum wsLeft
            in map (max 0 . subtract 1) $ wsLeft ++ [wCurrent + 1] ++ upto wRest rgt
  | otherwise = over (taking n traversed) (+ (dw+1))
              $ over (dropping n traversed) (+ dw) ws
  where ws1 = map succ ws
        s = sum ws1
        l = length ws1
        (dw, n) = (w - s) `divMod` l
        upto _ [] = []
        upto ml (x:xs) = v:upto (ml-v) xs
            where v = min ml x

withWidths :: (a -> Widget Name) -> [Int] -> [a] -> [Widget Name]
withWidths f ws l = intersperse vBorder
                  $ map (uncurry $ withWidth f)
                  $ filter ((>0) . fst)
                  $ zip ws l

withWidth :: (a -> Widget Name) -> Int -> a -> Widget Name
withWidth f w = hLimit w . padRight Max . f

renderName :: Bool -> Text -> Widget Name
renderName = (withAttr titleAttr .) . renderElement


renderValue :: Bool -> Text -> Widget Name
renderValue = renderElement

renderValueEditor :: ValueEditor -> Widget Name
renderValueEditor ve = Ed.renderEditor ((if ve ^. veIsError
                        then withAttr errorAttr
                        else id) . (txt . T.unlines)) True $ ve ^. veEditor

renderElement :: Bool -> Text -> Widget Name
renderElement False = myTxt
renderElement True = withAttr selectedElementAttr . myTxt


myTxt :: Text -> Widget n
myTxt t = Widget Fixed Fixed $ do
      w <- availWidth <$> getContext
      let l = T.length t
          t' | l <= w = T.justifyLeft w ' ' t
             | otherwise = T.take (w-3) t <> "..."
      render $ txt t'
