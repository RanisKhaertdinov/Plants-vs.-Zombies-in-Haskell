module GameStates where

import Plant
import LawnMower
import GameTypes (Zombie, Bullet)
import GHC.IO.Exception (IOErrorType(HardwareFault))
import System.Random (StdGen)

-- | Represents a falling or collectible sun.
data Sun = Sun
    { startPos :: (Float, Float)  -- ^ Where the sun starts falling from
    , endPos :: (Float, Float)    -- ^ Where the sun lands
    , value :: Int                -- ^ Sun value (currency)
    , bornTime :: Float           -- ^ When the sun was created
    , fallProgress :: Float       -- ^ 0 (just started) to 1 (landed)
    } deriving (Show)


data GameDifficult
  = Easy
  | Medium
  | Hard
  | Boss
  deriving (Show)

type Time = Float
type SunCount = Int
type WaveNumber = Int
type RandomSunTimer = Float
type SunTimers = ((Float, Float), Float )
-- | The main game state, including all entities and the current wave.
data GameState
    = Playing [Plant] Time (Maybe PlantType) SunCount [Sun] [SunTimers] [LawnMower] [Zombie] [Bullet] WaveNumber GameDifficult StdGen RandomSunTimer
      -- ^ Playing: plants, time, selected plant, sun, suns, sunTimers, mowers, zombies, bullets, wave, random generator, random sun timer
    | GameOver  -- ^ The player has lost
    | Win       -- ^ The player has won
    deriving (Show)