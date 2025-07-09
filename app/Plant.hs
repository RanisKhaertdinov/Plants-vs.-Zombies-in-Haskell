module Plant where

import Graphics.Gloss



data Plant  -- (dx, dy) health
    = Plant PlantType (Float, Float) Int

data PlantType
    = Sunflower
    | Peashooter


generatePlant :: Plant -> Picture 
generatePlant (Plant Sunflower (x, y) health) = 
    if health > 0 
    then Translate x y $ Color yellow $ circleSolid 30
    else blank
generatePlant (Plant Peashooter (x, y) health) = 
    if health > 0 
    then Translate x y $ Color green $ circleSolid 30
    else blank





