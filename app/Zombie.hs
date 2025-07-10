module Zombie where

import Graphics.Gloss

type Start = Float
type Lane = Float
type Speed = Float
type Coord = (Float, Float)
type HitboxSize = (Float, Float)

data Position = Position Start Lane Speed Coord HitboxSize

type Health = Int
data Coloring = Coloring Float Float Float Float

data Zombie = Zombie Position Health Coloring

animateZombie :: Zombie -> Float -> Picture
animateZombie (Zombie (Position start lane speed (x, y) (hx, hy)) hp (Coloring r g b a)) time
    = Translate (-speed*time + start) ((lane-2)*66.6) (Color (makeColor r g b a) $ circleSolid 30)

animateAllZ :: [Zombie] -> Float -> [Picture]
animateAllZ [] _ = []
animateAllZ (x:xs) time = (animateZombie x time) : animateAllZ xs time

updateZombie :: Zombie -> Float -> Zombie
updateZombie (Zombie (Position start lane speed (x, y) (hx, hy)) hp (Coloring r g b a)) time
    = (Zombie (Position start lane speed ((-speed*time + start), ((lane-2)*66.6)) (hx, hy)) hp (Coloring r g b a))

updateAllZ :: [Zombie] -> Float -> [Zombie]
updateAllZ [] _ = []
updateAllZ (x:xs) time = (updateZombie x time) : updateAllZ xs time



hitZombie :: Zombie -> Int -> Zombie
hitZombie (Zombie pos hp col) damage 
    | hp-damage > 0 = Zombie pos (hp-damage) col
    | otherwise = Zombie pos 0 (Coloring 0 0 0 0.8)

clearDead :: [Zombie] -> [Zombie]
clearDead [] = []
clearDead ((Zombie pos hp col):xs) 
    | hp > 0   = (Zombie pos hp col) : clearDead xs
    | otherwise = clearDead xs

checkFinish :: [Zombie] -> Float -> Bool
checkFinish [] _ = False
checkFinish ((Zombie (Position _ _ _ (x,y) _) _ _):xs) edge = (x<edge) || checkFinish xs edge


-- generateZombie :: Zombie -> Picture 
-- generateZombie (Zombie pos hp (Coloring r g b a))
--     | hp > 0 = Color (makeColor r g b a) $ circleSolid 30
--     | otherwise = blank
