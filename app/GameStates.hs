module GameStates where

import Plant

data GameState = Playing Float | GameOver | SelectingPlant PlantType
    deriving (Show)
