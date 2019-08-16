{-# LANGUAGE OverloadedLabels #-}

module GUI.BuildMonad (
  -- *Types
  BuildMonad
  -- *Low level actions
  , runBuild
  , getBuilder
  , getControl
  , getObject
  , ioVoid
  -- *General actions
  , buttonAction
  , buttons
  , menuItemAction
  , menuItemInput
  -- *Rexported
  , GUIControl(..)
) where

import Control.Monad.Reader(void, liftIO, asks, ReaderT, runReaderT)
import Data.Maybe(fromJust)
import Data.Text(Text)
import GI.Gtk(on, MenuItem, Button, builderGetObject, Builder)

import GUI.CanBeCast
import GUI.Control
import Presenter.Input

type BuildMonad = ReaderT (Builder, GUIControl) IO

runBuild :: Builder -> GUIControl -> BuildMonad t -> IO t
runBuild builder control action = runReaderT action (builder, control)

getBuilder :: BuildMonad Builder
getBuilder = asks fst

getControl :: BuildMonad GUIControl
getControl = asks snd

getObject :: CanBeCast obj => Text -> BuildMonad obj
getObject s = do
    builder <- getBuilder
    liftIO $ builderGetObject builder s >>= doCast . fromJust

ioVoid :: IO a -> BuildMonad ()
ioVoid = liftIO . void

buttonAction :: IsInput cmd => Text -> cmd -> BuildMonad ()
buttonAction name input = do
    control <- getControl
    btn <- getObject name
    ioVoid ((btn :: Button) `on` #clicked $ sendInput control input)

buttons :: IsInput cmd => [(Text, cmd)] -> BuildMonad ()
buttons = mapM_ (uncurry buttonAction)

menuItemInput :: IsInput cmd => Text -> cmd -> BuildMonad ()
menuItemInput name input = do
    control <- getControl
    menuItemAction name $ sendInput control input

menuItemAction :: Text -> IO () -> BuildMonad ()
menuItemAction name io = do
    control <- getControl
    itm <- getObject name
    ioVoid ((itm :: MenuItem) `on` #activate $ io)


