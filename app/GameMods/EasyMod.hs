module GameMods.EasyMod where

import GameTypes (Zombie(..), Position(..), Coloring(..))

initialSunsCount :: Int
initialSunsCount = 750

zombieHealthMod :: Float
zombieHealthMod = 1.0

zombieSpeedMod :: Float
zombieSpeedMod = 1.0

sunIntervalMod :: Float
sunIntervalMod = 1.0



waveZombieCount :: Int
waveZombieCount = 2

-- Generate zombies for the next wave (Easy difficulty)
generateWave :: Int -> [Zombie]
generateWave waveNum =
  [ Zombie (Position x lane (speed * zombieSpeedMod) (x, gridY !! laneIdx) (30, 30)) (round (hp * zombieHealthMod)) (Coloring 1 1 1 1) False
  | (x, laneIdx, speed, hp) <-
      case waveNum of
        0 -> [(900, 0, 30, 8), (950, 1, 28, 8), (1000, 2, 32, 8)] -- Initial wave
        1 -> [(900, 0, 32, 10), (950, 1, 30, 10), (1000, 2, 34, 10), (1050, 3, 31, 10)]
        2 -> [(900, 0, 32, 10), (950, 1, 30, 10), (1000, 2, 34, 10), (1050, 3, 31, 10)]
        _ -> [] -- No more waves for Easy
  , let lane = fromIntegral laneIdx
  ]
  where
    gridY = [fromIntegral i * 66.6 - (66.6 * 2) | i <- [0..4] :: [Int]]