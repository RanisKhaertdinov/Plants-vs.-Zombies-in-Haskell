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
animateBullet (Bullet (Position start lane speed (x,y) _) _ (Coloring r g b a))
    = Translate x ((lane-2)*66.6) (Color (makeColor r g b a) $ circleSolid 5)

animateAllB :: [Bullet] -> [Picture]
animateAllB [] = []
animateAllB (x:xs) = animateBullet x : animateAllB xs

updateBullet :: Bullet -> Float -> Bullet
updateBullet (Bullet (Position start lane speed _ (hx, hy)) dmg (Coloring r g b a)) time
    = Bullet (Position start lane speed (- (speed * time) + start, (lane-2)*66.6) (hx, hy)) dmg (Coloring r g b a)

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
    | mod' time 2 == 0     = Bullet (Position x (posToLane y) 200 (x, y) (5, 5)) 3 (Coloring 0 1 0 1) : bullets
    | otherwise             = bullets
conjureBullet _ _ b = b

conjureAll :: [Plant] -> Float -> [Bullet] -> [Bullet]
conjureAll [] _ bullets = bullets
conjureAll (x:xs) t bullets = conjureAll xs t (conjureBullet x t bullets)




checkCollision :: Zombie -> Bullet -> Bool
checkCollision (Zombie (Position _ lane1 _ (x1, _) (hx1, _)) _ _) (Bullet (Position _ lane2 _ (x2, _) (hx2, _)) _ _)
    = lane1 == lane2 && x2 - x1 < 0

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
