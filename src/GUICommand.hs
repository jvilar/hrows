module GUICommand(
                  GUICommand(..)
                 , module Iteration
                  ) where


import Iteration

data GUICommand = ShowPosition Int Int
                | ShowRow [String]
                | ShowNames [String]
                | ShowIteration Iteration
                deriving Show
