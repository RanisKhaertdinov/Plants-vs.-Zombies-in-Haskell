module RandomSuns where

import System.Random (StdGen, randomR)
import GameStates (Sun(..))
import LittleSun

-- Minimum and maximum X coordinates for random sun spawn
sunXMin, sunXMax :: Float
sunXMin = -300
sunXMax = 190

-- Minimum and maximum Y coordinates for random sun spawn
-- Suns will fall from a random Y position between sunYStart and sunYEnd
sunYStart, sunYEnd :: Float
sunYStart = 200
sunYEnd = -100

-- Interval (in seconds) between random sun spawns
randomSunInterval :: Float
randomSunInterval = 7

-- | Generate a random Sun object at a random position.
-- The sun will start at a random (X, Y) within the defined bounds and fall downwards.
-- The random generator is threaded through the function for pure randomness.
getRandomSun :: Float -> StdGen -> (Sun, StdGen)
getRandomSun t gen =
  let (startX, gen1) = randomR (sunXMin, sunXMax) gen
      (startY, gen2) = randomR (sunYStart, sunYEnd) gen1  -- Use full Y range for random spawn
      endX = startX
      endY = startY - 70
      sun = createSun (startX, startY) (endX, endY) t
  in (sun, gen2)