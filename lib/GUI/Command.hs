module GUI.Command(
                  GUICommand(..)
                  , FieldInfo(..)
                  , module GUI.Iteration
                  ) where


import Data.Text(Text)
import GHC.Int(Int32)
import GUI.Iteration

import Model
import Model.Expression
import Model.Field

data GUICommand = ChangeTitle Text
                | ShowPosition Int Int
                | ShowFields [FieldInfo]
                | ShowNames [FieldName]
                | ShowIteration Iteration
                | DisableTextViews
                | ShowListing
                | HideListing
                | CompleteListing [[Text]]
                deriving Show

data FieldInfo = FieldInfo { indexFI :: FieldPos
                           , textFI :: Maybe Text
                           , formulaFI :: Maybe Formula
                           , typeFI :: FieldType
                           , isErrorFI :: Bool
                           } deriving Show
