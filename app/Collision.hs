module Collision where

import GameTypes (Position(..), Zombie(..), posCoord, posLane)
import LawnMower (LawnMower(..))
import Debug.Trace

checkCollision :: Zombie -> LawnMower -> Bool
checkCollision z m =
  let (zx, zy) = posCoord (zombiePos z)
      my = lawnLane m * 66.6 - 2 * 66.6
      laneDiff = abs (posLane (zombiePos z) - lawnLane m)
      collides = abs (zx - lawnPos m) < 10 && abs (zy - my) < 50 && laneDiff < 0.1
  in trace (show (zx, zy, lawnPos m, my, posLane (zombiePos z), lawnLane m, collides)) collides