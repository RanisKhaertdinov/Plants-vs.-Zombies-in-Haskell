module GameStates where

import Types (PlantType)

data GameState = Playing Float | GameOver | SelectingPlant PlantType
    deriving (Show)
