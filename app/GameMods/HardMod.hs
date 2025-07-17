module GameMods.HardMod where

import GameTypes (Zombie(..), Position(..), Coloring(..))

initialSunsCount :: Int
initialSunsCount = 750


zombieHealthMod :: Float
zombieHealthMod = 1.5

zombieSpeedMod :: Float
zombieSpeedMod = 1.0

sunIntervalMod :: Float
sunIntervalMod = 0.8

waveZombieCount :: Int
waveZombieCount = 4

-- Generate zombies for the next wave (Hard difficulty)
generateWave :: Int -> [Zombie]
generateWave waveNum =
  [ Zombie (Position x lane (speed * zombieSpeedMod) (x, gridY !! laneIdx) (30, 30)) (round (hp * zombieHealthMod)) (Coloring 1 1 1 1) False
  | (x, laneIdx, speed, hp) <-
      case waveNum of
        0 -> [(900, 0, 40, 15), (950, 1, 38, 15), (1000, 2, 42, 15), (1050, 3, 39, 15), (1100, 4, 41, 15)] -- Initial wave
        1 -> [(900, 0, 42, 18), (950, 1, 40, 18), (1000, 2, 44, 18), (1050, 3, 41, 18), (1100, 4, 43, 18)]
        2 -> [(900, 0, 45, 20), (950, 1, 43, 20), (1000, 2, 47, 20), (1050, 3, 44, 20), (1100, 4, 46, 20)]
        3 -> [(900, 0, 48, 25), (950, 1, 46, 25), (1000, 2, 50, 25), (1050, 3, 47, 25), (1100, 4, 49, 25)]
        4 -> [(900, 0, 48, 25), (950, 1, 46, 25), (1000, 2, 50, 25), (1050, 3, 47, 25), (1100, 4, 49, 25)]
        _ -> [] -- No more waves for Hard
  , let lane = fromIntegral laneIdx
  ]
  where
    gridY = [fromIntegral i * 66.6 - (66.6 * 2) | i <- [0..4] :: [Int]]