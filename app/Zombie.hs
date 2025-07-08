module Zombie (generateZombie) where

import Graphics.Gloss

generateZombie :: Picture 
generateZombie = Color (makeColor 0.0 0.0 1.0 0.8) $ circleSolid 30


