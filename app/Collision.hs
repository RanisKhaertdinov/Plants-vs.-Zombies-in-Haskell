module Collision where

import GameTypes (Position(..), Zombie(..), posCoord, posLane)
import LawnMower (LawnMower(..))

checkCollision :: Zombie -> LawnMower -> Bool
checkCollision z m =
  let (zx, zy) = posCoord (zombiePos z)
      my = lawnLane m * 66.6 - 2 * 66.6
  in abs (zx - lawnPos m) < 30 && abs (zy - my) < 10 && posLane (zombiePos z) == lawnLane m