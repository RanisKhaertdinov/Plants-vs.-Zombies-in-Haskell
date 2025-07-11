module GameStates where

import Plant
import LawnMower

data GameState
    = Playing [Plant] Float Int [LawnMower]
    | GameOver
    | SelectingPlant [Plant] Float PlantType Int [LawnMower]
    deriving (Show)