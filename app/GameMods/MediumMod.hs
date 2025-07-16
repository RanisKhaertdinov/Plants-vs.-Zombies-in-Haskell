module GameMods.MediumMod where

import GameTypes (Zombie(..), Position(..), Coloring(..))

initialSunsCount :: Int
initialSunsCount = 750

zombieHealthMod :: Float
zombieHealthMod = 1.2

zombieSpeedMod :: Float
zombieSpeedMod = 1.1

sunIntervalMod :: Float
sunIntervalMod = 0.9


waveZombieCount :: Int
waveZombieCount = 3

-- Generate zombies for the next wave (Medium difficulty)
generateWave :: Int -> [Zombie]
generateWave waveNum =
  [ Zombie (Position x lane (speed * zombieSpeedMod) (x, gridY !! laneIdx) (30, 30)) (round (hp * zombieHealthMod)) (Coloring 1 1 1 1)
  | (x, laneIdx, speed, hp) <-
      case waveNum of
        0 -> [(900, 0, 35, 10), (950, 1, 33, 10), (1000, 2, 37, 10), (1050, 3, 34, 10)] -- Initial wave
        1 -> [(900, 0, 37, 12), (950, 1, 35, 12), (1000, 2, 39, 12), (1050, 3, 36, 12), (1100, 4, 38, 12)]
        2 -> [(900, 0, 40, 15), (950, 1, 38, 15), (1000, 2, 42, 15), (1050, 3, 39, 15), (1100, 4, 41, 15)]
        3 -> [(900, 0, 40, 15), (950, 1, 38, 15), (1000, 2, 42, 15), (1050, 3, 39, 15), (1100, 4, 41, 15)]
        _ -> [] -- No more waves for Medium
  , let lane = fromIntegral laneIdx
  ]
  where
    gridY = [fromIntegral i * 66.6 - (66.6 * 2) | i <- [0..4] :: [Int]]