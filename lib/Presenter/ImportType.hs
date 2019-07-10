module Presenter.ImportType (
  -- *Types
  ImportType(..)
) where

-- |The two kind of imports in the application.
data ImportType = ImportFields | ImportRows deriving Show
