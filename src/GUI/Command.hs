module GUI.Command(
                  GUICommand(..)
                  , FieldInfo(..)
                  , module GUI.Iteration
                  ) where


import GUI.Iteration

data GUICommand = ChangeTitle String
                | ShowPosition Int Int
                | ShowFields [FieldInfo]
                | ShowNames [String]
                | ShowIteration Iteration
                | DisableTextViews
                deriving Show

data FieldInfo = FieldInfo { indexFI :: Int
                           , textFI :: String
                           , isFormulaFI :: Bool
                           , isErrorFI :: Bool
                           } deriving Show
