module GameMods.BossMod where

import GameTypes (Zombie(..), Position(..), Coloring(..))
import qualified Zombie as Z

initialSunsCount :: Int
initialSunsCount = 500

zombieHealthMod :: Float
zombieHealthMod = 1.0

zombieSpeedMod :: Float
zombieSpeedMod = 1.0

sunIntervalMod :: Float
sunIntervalMod = 0.7

waveZombieCount :: Int
waveZombieCount = 1

generateWave :: Int -> [Zombie]
generateWave waveNum =
  [ Zombie (Position x lane (speed * zombieSpeedMod) (x, gridY !! laneIdx) (100, 300)) (round (hp * zombieHealthMod)) (Coloring 0.8 0.2 0.2 1) True
  | (x, laneIdx, speed, hp) <- case waveNum of
      0 -> [(450, 2, 20, 150)]  -- Single Boss zombie in lane 0
      _ -> []                    -- No other waves
  , let lane = fromIntegral laneIdx
  ]
  where
    gridY = [fromIntegral i * 66.6 - (66.6 * 2) | i <- [0..4] :: [Int]]