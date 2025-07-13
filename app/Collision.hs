module Collision (checkCollision) where

import GameTypes

checkCollision :: Zombie -> LawnMower -> Bool
checkCollision z m =
  abs (fst (posCoord (zombiePos z)) - lawnPos m) < 25 &&  -- Проверка по X-координате
  posLane (zombiePos z) == lawnLane m