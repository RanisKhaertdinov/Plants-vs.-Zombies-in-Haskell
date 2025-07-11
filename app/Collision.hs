module Collision (checkCollision) where

import GameTypes

checkCollision :: Zombie -> LawnMower -> Bool
checkCollision (Zombie pos _ _) (LawnMower lwLane lwPos _ _) =
    let (x, _) = posCoord pos
    in posLane pos == lwLane && x <= lwPos + 25 && x >= lwPos - 25