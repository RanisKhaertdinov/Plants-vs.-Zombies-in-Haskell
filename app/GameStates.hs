module GameStates where

data GameState = Playing Float | GameOver | SelectingPlant PlantType
