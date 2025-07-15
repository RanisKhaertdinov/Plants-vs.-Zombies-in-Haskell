module Zombie
    ( Zombie(..)
    , animateAllZ
    , updateZombieStep
    , checkFinish
    , hitZombie
    , clearDead
    , prettyZombie
    , onlawn
    ) where

import Graphics.Gloss
import GameTypes
import Data.Maybe (mapMaybe)



slow :: Float
slow = 8

animateZombie :: Zombie -> Picture
animateZombie (Zombie pos _ _) =
    let (x, y) = posCoord pos
    in Translate x y prettyZombie

animateAllZ :: [Zombie] -> [Picture]
animateAllZ zombies =
  [ animateZombie z
  | z@(Zombie pos health _) <- zombies
  , health > 0
  ]

updateZombieStep :: Zombie -> Float -> Zombie
updateZombieStep (Zombie (Position start lane speed (x, y) (hx, hy)) hp col) dt =
    let newX = x - speed * dt
    in Zombie (Position start lane speed (newX, y) (hx, hy)) hp col

checkFinish :: [Zombie] -> Float -> Bool
checkFinish zombies edge = any isAtEdge zombies
    where
        isAtEdge (Zombie (Position _ _ _ (x, _) _) health _) = health > 0 && x <= edge


hitZombie :: Zombie -> Int -> Zombie
hitZombie (Zombie pos hp col) damage
    | hp-damage > 0 = Zombie pos (hp-damage) col
    | otherwise = Zombie pos 0 (Coloring 0 0 0 0.8)

onlawn :: Zombie -> Bool
onlawn (Zombie (Position _ _ _ (x, _) _) _ _) = x<500

clearDead :: [Zombie] -> [Zombie]
clearDead [] = []
clearDead ((Zombie pos hp col):xs)
    | hp > 0   = (Zombie pos hp col) : clearDead xs
    | otherwise = clearDead xs

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