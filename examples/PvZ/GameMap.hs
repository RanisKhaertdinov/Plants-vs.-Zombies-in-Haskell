module GameMap (myMap) where

import Graphics.UI.Fungen.Map

width = 1
height = 1

tile = (0, True, 1.0, ())
row = replicate width tile
matrix = replicate height row

myMap :: GameMap ()
myMap = tileMap matrix 550 300

