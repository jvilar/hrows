{-# LANGUAGE TypeFamilies #-}

module GUI.HKD (
  HKD(..)
  ) where

import Data.Functor.Identity(Identity)

type family HKD f a where
  HKD Identity a = a
  HKD f a = f a
