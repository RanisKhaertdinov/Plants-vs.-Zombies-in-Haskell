module GameStates where

import Plant
import LawnMower
import GameTypes (Zombie, Bullet)

data Sun = Sun
    { startPos :: (Float, Float)
    , endPos :: (Float, Float)
    , value :: Int
    , bornTime :: Float
    , fallProgress :: Float
    } deriving (Show)

data GameState
    = Playing [Plant] Float Int [Sun] [((Float,Float), Float)] [LawnMower] [Zombie] [Bullet]
    | GameOver
    | Win
    | SelectingPlant [Plant] Float PlantType Int [Sun] [((Float,Float), Float)] [LawnMower] [Zombie] [Bullet]
    deriving (Show)