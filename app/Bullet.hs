module Bullet
    ( Bullet (..)
    , animateAllB
    , updateAllB
    , conjureAll
    , checkAllB
    , hitAllZAllB
    , exhaustBullets
    ) where


import Graphics.Gloss
import Plant
import GameStates
import GameTypes
import Data.Fixed (mod')
import GHC.IO.Buffer (Buffer)


-- generateBullet :: Plant -> Float -> GameState-> Picture
-- generateBullet (Plant Peashooter (x, y) _) time gameMod =
--     case gameMod of
--         Playing  {} ->
--             let bulSpeed = 200
--                 distance = 600 - x
--                 interval = 1.5
--                 bulletTimes = [time, time - interval, time - 2 * interval]
--                 bulletPics = [ let t = mod' (bulSpeed * t') distance
--                                    bulX = x + t
--                                in if t' >= 0
--                                   then Translate bulX y $ Color green $ circleSolid 10
--                                   else blank
--                              | t' <- bulletTimes
--                            ]

--             in Pictures bulletPics
--         GameOver -> blank
--         SelectingPlant  {}-> blank
-- generateBullet _ _ _ = blank


animateBullet :: Bullet -> Picture
animateBullet (Bullet pos _ (Coloring r g b a)) =
    let (x, y) = posCoord pos
    in Translate x y (Color (makeColor r g b a) $ circleSolid 5)

animateAllB :: [Bullet] -> [Picture]
animateAllB bullets =
  [ animateBullet b
  | b@(Bullet pos dmg _) <- bullets
  ]

updateBullet :: Bullet -> Float -> Bullet
updateBullet (Bullet (Position start lane speed (x, y) (hx, hy)) dmg (Coloring r g b a)) dt
    = Bullet (Position start lane speed ((speed * dt) + x, y) (hx, hy)) dmg (Coloring r g b a)

updateAllB :: [Bullet] -> Float -> [Bullet]
updateAllB [] _ = []
updateAllB (x:xs) time = updateBullet x time : updateAllB xs time

roundFloat :: Float -> Float
roundFloat x = x - rem x
    where
        rem :: Float -> Float
        rem x
            | x < 0             = rem (x+1)
            | x >= 1            = rem (x-1)
            | x >= 0 && x < 1   = x
            | otherwise         = x

posToLane :: Float -> Float
posToLane y = roundFloat ((y/66.6)+2)

conjureBullet :: Plant -> Float -> [Bullet] -> [Bullet]
conjureBullet (Plant Peashooter (x, y) _) time bullets
    | (roundFloat time) `mod'` 3 == 0     = Bullet (Position x (posToLane y) 200 (x, y) (5, 5)) 3 (Coloring 0 1 0 1) : bullets
    | otherwise             = bullets
conjureBullet _ _ b = b

conjureAll :: [Plant] -> Float -> [Bullet] -> [Bullet]
conjureAll [] _ bullets = bullets
conjureAll (x:xs) t bullets = conjureAll xs t (conjureBullet x t bullets)

getLane :: Bullet -> Float
getLane (Bullet (Position _ l _ _ _) _ _) = l

getX :: Bullet -> Float
getX (Bullet (Position _ _ _ (x,_) _) _ _) = x

checkCollision :: Zombie -> Bullet -> Bool
checkCollision z b =
  let (zx, zy) = posCoord (zombiePos z)
      my = getLane b * 66.6 - 2 * 66.6
      laneDiff = abs (posLane (zombiePos z) - getLane b)
      collides = abs (zx - getX b) < 10 && abs (zy - my) < 50 && laneDiff < 0.1
  in collides

-- checkCollision :: Zombie -> Bullet -> Bool
-- checkCollision (Zombie (Position _ lane1 _ (x1, _) (hx1, _)) _ _) (Bullet (Position _ lane2 _ (x2, _) (hx2, _)) _ _)
--     = lane1 == lane2 && x2 - x1 < 0

checkAllB :: [Zombie] -> Bullet -> Bool
checkAllB [] _ = False
checkAllB (z:zs) b = checkCollision z b || checkAllB zs b



hitZombie :: Zombie -> Int -> Zombie
hitZombie (Zombie pos hp col) damage
    | hp-damage > 0 = Zombie pos (hp-damage) col
    | otherwise = Zombie pos 0 (Coloring 0 0 0 0.8)

hitAllZOneB :: [Zombie] -> Bullet -> [Zombie]
hitAllZOneB [] _ = []
hitAllZOneB (x:xs) y@(Bullet pos dmg col)
    | checkCollision x y   = hitZombie x dmg : hitAllZOneB xs y
    | otherwise              = x : hitAllZOneB xs y

hitAllZAllB :: [Zombie] -> [Bullet] -> [Zombie]
hitAllZAllB z [] = z
hitAllZAllB z (b:bs) = hitAllZAllB (hitAllZOneB z b) bs



exhaustBullets :: [Bullet] -> [Zombie] -> [Bullet]
exhaustBullets [] _ = []
exhaustBullets (b:bs) zs
    | checkAllB zs b = exhaustBullets bs zs
    | otherwise = b : exhaustBullets bs zs
