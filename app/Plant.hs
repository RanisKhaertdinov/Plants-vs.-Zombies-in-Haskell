module Plant (generatePlant) where

import Graphics.Gloss

generatePlant :: Picture 
generatePlant = Color (makeColor 1.0 1.0 1.0 1) $ circleSolid 30


