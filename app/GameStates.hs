module GameStates where

import Plant
import LawnMower
import GameTypes (Zombie, Bullet)

-- | Represents a falling or collectible sun.
data Sun = Sun
    { startPos :: (Float, Float)  -- ^ Where the sun starts falling from
    , endPos :: (Float, Float)    -- ^ Where the sun lands
    , value :: Int                -- ^ Sun value (currency)
    , bornTime :: Float           -- ^ When the sun was created
    , fallProgress :: Float       -- ^ 0 (just started) to 1 (landed)
    } deriving (Show)

-- | The main game state, including all entities and the current wave.
data GameState
    = Playing [Plant] Float (Maybe PlantType) Int [Sun] [((Float,Float), Float)] [LawnMower] [Zombie] [Bullet] Int
      -- ^ Playing: plants, time, selected plant, sun, suns, sunTimers, mowers, zombies, bullets, wave
    | GameOver  -- ^ The player has lost
    | Win       -- ^ The player has won
    deriving (Show)