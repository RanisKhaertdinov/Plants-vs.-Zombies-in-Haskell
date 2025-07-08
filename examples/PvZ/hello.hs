module Main where

import Graphics.UI.Fungen
import GameMap (myMap)
import Graphics.Rendering.OpenGL (GLdouble)
import Paths_FunGEn (getDataFileName)

main :: IO ()
main = do
  texbmp <- getDataFileName "examples/PvZ/Frontyard.bmp"
  let winConfig = ((50,50),(500,300),"Plants vs Zombies in Haskell! Press Q to quit")
      bindings = [(Char 'q', Press, \_ _ -> funExit)]
      bmpList = [(texbmp, Nothing)]
  funInit winConfig myMap [] () () bindings (return()) Idle bmpList
   
