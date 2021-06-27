{-# LANGUAGE OverloadedStrings #-}
module Presenter.DialogAuto (
              -- *Functions
              dialogAuto
) where


import Control.Auto(arrM)
import qualified Data.Text as T

import GUI.Command
import Model
import Presenter.Auto
import Presenter.Input

dialogAuto :: PresenterAuto (DialogCommand, Model, RowPos) ()
dialogAuto = arrM $ \(input, model, pos) -> let
                           fieldNames = fnames <@ model
                           fieldVisibilities = visibilities <@ model
                           sourceList = [(getName rst, siFilePath si, fnames rst)
                                         | (rst, si) <- zip (getRowStores <@ model)
                                                            (getSourceInfos model)
                                        ]
                        in sendGUIM . ShowIteration $ case input of
                                             LoadFileDialog -> AskReadFile
                                             SaveAsFileDialog -> AskWriteFile
                                             CreateFieldsDialog -> AskCreateField
                                             DeleteFieldsDialog -> AskDeleteFields fieldNames
                                             ImportFromDialog t -> AskImportFrom t
                                             ChooseImportDialog t rst -> AskImportOptions t (fnames rst) fieldNames rst
                                             RenameFieldsDialog -> AskRenameFields fieldNames
                                             ShowHideFieldsDialog -> AskShowHideFields fieldNames fieldVisibilities
                                             SortRowsDialog -> AskSortRows fieldNames
                                             MessageDialog m -> DisplayMessage m
                                             ChangeFieldFormulaDialog f -> GetFieldFormula f (fieldNames !!! f) (formulas `from` model !!! f)
                                             SearchFieldDialog f -> SearchField f (toString $ row pos `from` model !!! f) (fieldValues f `from` model)
                                             CopyOtherDialog f -> CopyOtherField f (toString $ row pos `from` model !!! f) (fieldValues f `from` model)
                                             AddSourceDialog -> AskAddSource
                                             ShowSourcesDialog -> ShowSources sourceList
                                             RenameSourcesDialog -> AskRenameSources (map fst3 sourceList)
                                             ShowAboutDialog -> DisplayAbout
                                             d -> DisplayMessage . ErrorMessage $ T.concat ["Error: ", T.pack $ show d, " not implemented"] 

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a
