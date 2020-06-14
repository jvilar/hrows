{-# LANGUAGE OverloadedStrings #-}
module Presenter.DialogAuto (
              -- *Functions
              dialogAuto
) where


import Control.Auto(arrM)
import qualified Data.Text as T

import GUI.Command
import Model
import Model.RowStore
import Presenter.Auto
import Presenter.Input

dialogAuto :: PresenterAuto (DialogCommand, Model, RowPos) ()
dialogAuto = arrM $ \(input, model, pos) -> let
                           fieldNames = fnames <@ model
                           sourceList = [(getName rst, fnames rst) | rst <- getRowStores <@ model ]
                        in sendGUIM . ShowIteration $ case input of
                                             LoadFileDialog -> AskReadFile
                                             SaveAsFileDialog -> AskWriteFile
                                             CreateFieldsDialog -> AskCreateField
                                             DeleteFieldsDialog -> AskDeleteFields fieldNames
                                             ImportFromDialog t -> AskImportFrom t
                                             ChooseImportDialog t rst -> AskImportOptions t (fnames rst) fieldNames rst
                                             ChangeNamesDialog -> AskRenameFields fieldNames
                                             SortRowsDialog -> AskSortRows fieldNames
                                             MessageDialog m -> DisplayMessage m
                                             ChangeFieldFormulaDialog f -> GetFieldFormula f (fieldNames !!! f) (formulas `from` model !!! f)
                                             SearchFieldDialog f -> SearchField f (toString $ row pos `from` model !!! f) (fieldValues f `from` model)
                                             CopyOtherDialog f -> CopyOtherField f (toString $ row pos `from` model !!! f) (fieldValues f `from` model)
                                             AddSourceDialog -> AskAddSource
                                             ShowSourcesDialog -> ShowSources sourceList
                                             ShowAboutDialog -> DisplayAbout
                                             d -> DisplayMessage . ErrorMessage $ T.concat ["Error: ", T.pack $ show d, " not implemented"] 