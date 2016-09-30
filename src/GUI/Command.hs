module GUI.Command(
                  GUICommand(..)
                  , FieldState(..)
                  , module GUI.Iteration
                  ) where


import GUI.Iteration

data GUICommand = ShowPosition Int Int
                | ShowRow [(String, FieldState)]
                | ShowNames [String]
                | ShowIteration Iteration
                | ShowFieldState Int FieldState
                | DisableTextViews
                deriving Show

data FieldState = NormalFieldState
                | ErrorFieldState
                deriving Show
