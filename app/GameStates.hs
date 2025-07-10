module GameStates where

import Plant

data GameState 
    = Playing [Plant] Float
    | GameOver 
    | SelectingPlant [Plant] Float PlantType
    deriving (Show)
