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

-- Bullet logic for Plants vs. Zombies in Haskell
-- Handles bullet creation, animation, movement, collision, and interaction with zombies

-- Use the same laneHeight as in Main.hs for lane calculations
laneHeight :: Float
laneHeight = 66.6  -- Must match Main.hs


animateBullet :: Bullet -> Picture
animateBullet (Bullet pos _ (Coloring r g b a)) =
    let (x, y) = posCoord pos
    in Translate x y (Color (makeColor r g b a) $ circleSolid 5)

-- Animate all bullets as Gloss pictures
animateAllB :: [Bullet] -> [Picture]
animateAllB bullets =
  [ animateBullet b
  | b@(Bullet pos dmg _) <- bullets
  ]

updateBullet :: Bullet -> Float -> Bullet
updateBullet (Bullet (Position start lane speed (x, y) (hx, hy)) dmg (Coloring r g b a)) dt
    = Bullet (Position start lane speed ((speed * dt) + x, y) (hx, hy)) dmg (Coloring r g b a)

tooLong :: Bullet -> Bool
tooLong (Bullet (Position start lane speed (x, y) (hx, hy)) dmg (Coloring r g b a)) 
    = x > 190 -- 500 край карты, 190 край газона

-- Update all bullets by dt, removing those that go too far
updateAllB :: [Bullet] -> Float -> [Bullet]
updateAllB [] _ = []
updateAllB (x:xs) time 
    | tooLong x = updateAllB xs time
    | otherwise = updateBullet x time : updateAllB xs time

roundFloat :: Float -> Float
roundFloat x = x - bRem x

bRem :: Float -> Float
bRem x
    | x < 0             = bRem (x+1)
    | x >= 1            = bRem (x-1)
    | x >= 0 && x < 1   = x
    | otherwise         = x

posToLane :: Float -> Float
posToLane y = roundFloat ((y/66.6)+2)

-- Create a bullet from a Peashooter if the time is right
conjureBullet :: Plant -> Float -> [Bullet] -> [Bullet]
conjureBullet (Plant Peashooter (x, y) _) time bullets
    | (bRem time < 0.1/3-0.01)     = Bullet (Position (x+20) (posToLane y) 200 (x+20, y) (5, 5)) 1 (Coloring 0 1 0 1) : bullets
    | otherwise             = bullets
conjureBullet _ _ b = b

-- Create bullets for all plants (mainly Peashooters)
conjureAll :: [Plant] -> Float -> [Bullet] -> [Bullet]
conjureAll [] _ bullets = bullets
conjureAll (x:xs) t bullets = conjureAll xs t (conjureBullet x t bullets)

getLane :: Bullet -> Float
getLane (Bullet (Position _ l _ _ _) _ _) = l

getX :: Bullet -> Float
getX (Bullet (Position _ _ _ (x,_) _) _ _) = x

-- Check if a bullet collides with any zombie
checkCollision :: Zombie -> Bullet -> Bool
checkCollision z b =
  let (zx, zy) = posCoord (zombiePos z)
      my = getLane b * laneHeight - 2 * laneHeight
      laneDiff = abs (posLane (zombiePos z) - getLane b)
      collides = abs (zx - getX b) < 10 && abs (zy - my) < 50 && (zombieHealth z >= 2000 || laneDiff < 0.1)
  in collides



-- Check if a bullet collides with any zombie
checkAllB :: [Zombie] -> Bullet -> Bool
checkAllB [] _ = False
checkAllB (z:zs) b = checkCollision z b || checkAllB zs b



hitZombie :: Zombie -> Int -> Zombie
hitZombie (Zombie pos hp col isBoss) damage
    | hp-damage > 0 = Zombie pos (hp-damage) col isBoss  -- Сохраняем статус босса
    | otherwise = Zombie pos 0 (Coloring 0 0 0 0.8) isBoss  -- Сохраняем статус даже после смерти

-- Apply bullet damage to all zombies for one bullet
hitAllZOneB :: [Zombie] -> Bullet -> [Zombie]
hitAllZOneB [] _ = []
hitAllZOneB (z@(Zombie pos hp col isBoss):zs) bullet
    | checkCollision z bullet =
        let damagedZombie = Zombie pos (max 0 (hp - bulletDamage bullet)) col isBoss
        in damagedZombie : hitAllZOneB zs bullet
    | otherwise = z : hitAllZOneB zs bullet

-- Apply all bullets to all zombies
hitAllZAllB :: [Zombie] -> [Bullet] -> [Zombie]
hitAllZAllB z [] = z
hitAllZAllB z (b:bs) = hitAllZAllB (hitAllZOneB z b) bs



-- Remove bullets that have hit a zombie
exhaustBullets :: [Bullet] -> [Zombie] -> [Bullet]
exhaustBullets [] _ = []
exhaustBullets (b:bs) zs
    | checkAllB zs b = exhaustBullets bs zs
    | otherwise = b : exhaustBullets bs zs
