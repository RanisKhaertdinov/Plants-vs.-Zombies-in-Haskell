module Plant
    ( PlantType(..)
    , Plant(..)
    , PlantCard(..)
    , availableCards
    , generatePlant
    ) where

import Graphics.Gloss



data Plant  -- (dx, dy) health
    = Plant PlantType (Float, Float) Int

data PlantType
    = Sunflower
    | Peashooter
    | WallNut
    deriving (Eq, Show)
    
data PlantCard = PlantCard
    { cardType :: PlantType
    , cost :: Int
    , cooldown :: Int
    }

availableCards :: [PlantCard]
availableCards =
    [ PlantCard Sunflower 50 7
    , PlantCard Peashooter 100 5
    , PlantCard WallNut 50 20
    ]


generatePlant :: Plant -> Picture 
generatePlant (Plant Sunflower (x, y) health) = 
    if health > 0 
    then Translate x y $ Color yellow $ circleSolid 30
    else blank
generatePlant (Plant Peashooter (x, y) health) = 
    if health > 0 
    then Translate x y $ Color green $ circleSolid 30
    else blank
generatePlant (Plant WallNut (x, y) health) =
    if health > 0
    then Translate x y $ Color (makeColor 0.5 0.3 0.1 1.0) $ rectangleSolid 40 60
    else blank




