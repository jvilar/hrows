module Main where

import Control.Auto.Run(runOnChan)
import Control.Concurrent(forkIO)
import Control.Concurrent.Chan(newChan)
import Control.Monad(void)
import Graphics.UI.Gtk (mainGUI, postGUIAsync)

import DisplayInfo
import GUI
import Presenter

main :: IO ()
main = do
  inputChan <- newChan
  control <- makeGUI inputChan
  updateGUI control display0
  forkIO $ void $ runOnChan (\info -> do
                                 postGUIAsync (updateGUI control info)
                                 print $ position info
                                 return True
                            )
                  inputChan
                  presenter
  mainGUI
