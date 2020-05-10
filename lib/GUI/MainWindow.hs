{-# LANGUAGE DeriveGeneric #-}

module GUI.MainWindow (
  -- *Types
  MainWindow
  , MainWindow'(..)
  ) where

import Control.Concurrent.Chan(Chan)
import Data.BitVector(BitVector)
import Data.IORef(IORef)
import GHC.Generics(Generic)
import GI.Gtk

import Presenter.Input

import GUI.HKD

data MainWindow' f = MainWindow { window :: HKD f Window
                                , positionLabel :: HKD f Label
                                , fieldsGrid :: HKD f Grid
                                , numberOfFields :: HKD f (IORef Int)
                                , currentField :: HKD f (IORef FieldPos)
                                , fieldMenu :: HKD f Menu
                                , beginButton :: HKD f Button
                                , endButton :: HKD f Button
                                , leftButton :: HKD f Button
                                , rightButton :: HKD f Button
                                , targetList :: HKD f TargetList
                                , textBufferActive :: HKD f (IORef BitVector)
                                , inputChanMW :: HKD f (Chan Input)
                                } deriving Generic

type MainWindow = MainWindow' Identity
