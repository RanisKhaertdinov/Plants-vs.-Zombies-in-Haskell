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

prettyZombie :: Picture
prettyZombie = Pictures
  [ -- Ноги
    Color (makeColorI 80 80 80 255) $ Translate (-7) (-28) $ rectangleSolid 6 18
  , Color (makeColorI 80 80 80 255) $ Translate (7) (-28) $ rectangleSolid 6 18
  , -- Тело
    Color (makeColorI 60 120 60 255) $ Translate 0 (-10) $ rectangleSolid 22 28
  , -- Рука левая
    Color (makeColorI 120 180 120 255) $ Translate (-18) (-5) $ Rotate 20 $ rectangleSolid 7 22
  , -- Рука правая
    Color (makeColorI 120 180 120 255) $ Translate (18) (-5) $ Rotate (-20) $ rectangleSolid 7 22
  , -- Голова
    Color (makeColorI 180 220 180 255) $ Translate 0 18 $ circleSolid 15
  , -- Глаз левый
    Color white $ Translate (-6) 24 $ circleSolid 3
  , Color black $ Translate (-6) 24 $ circleSolid 1.2
  , -- Глаз правый
    Color white $ Translate (6) 24 $ circleSolid 3
  , Color black $ Translate (6) 24 $ circleSolid 1.2
  , -- Рот
    Color (makeColorI 180 60 60 255) $ Translate 0 13 $ Scale 1 0.3 $ circleSolid 5
  ]
