module GameStates where

import Plant (PlantType(..))  -- Импорт из Plant.hs

data GameState = Playing Float | GameOver | SelectingPlant PlantType
    deriving (Show)