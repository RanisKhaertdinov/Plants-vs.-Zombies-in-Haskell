module GameStates where

import Plant (PlantType(..))

data GameState = Playing Float | GameOver | SelectingPlant PlantType
