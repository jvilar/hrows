module GUI.Command(
                  GUICommand(..)
                 , module GUI.Iteration
                  ) where


import GUI.Iteration

data GUICommand = ShowPosition Int Int
                | ShowRow [String]
                | ShowNames [String]
                | ShowIteration Iteration
                | DisableTextViews
                deriving Show
