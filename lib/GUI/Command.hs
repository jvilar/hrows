module GUI.Command(
                  GUICommand(..)
                  , FieldInfo(..)
                  , module GUI.Iteration
                  ) where


import GUI.Iteration
import Model.Expression
import Model.Field

data GUICommand = ChangeTitle String
                | ShowPosition Int Int
                | ShowFields [FieldInfo]
                | ShowNames [String]
                | ShowIteration Iteration
                | DisableTextViews
                deriving Show

data FieldInfo = FieldInfo { indexFI :: Int
                           , textFI :: Maybe String
                           , formulaFI :: Maybe Formula
                           , typeFI :: FieldType
                           , isErrorFI :: Bool
                           } deriving Show