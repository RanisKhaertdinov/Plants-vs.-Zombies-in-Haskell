module GameStates where

import Plant

data GameState
    = Playing [Plant] Float Int
    | GameOver
    | SelectingPlant [Plant] Float PlantType Int
    deriving (Show)