module Zombie
    ( animateZombie
    , animateAllZ
    , updateZombie
    , updateAllZ
    , hitZombie
    , clearDead
    , checkFinish
    ) where

import Graphics.Gloss
import GameTypes
import qualified Collision as C

animateZombie :: Zombie -> Float -> Picture
animateZombie (Zombie pos _ (Coloring r g b a)) _ =
    let (x, y) = posCoord pos
    in Translate x y (Color (makeColor r g b a) $ circleSolid 30)

animateAllZ :: [Zombie] -> Float -> [Picture]
animateAllZ zombies _ =
    [ animateZombie z 0 | z <- zombies, zombieHealth z > 0 ]

updateZombie :: Zombie -> Float -> Zombie
updateZombie (Zombie (Position _ lane speed coord _) hp col) time =
    let (x, y) = coord
        newX = x - speed * time
    in Zombie (Position newX lane speed (newX, y) (30, 30)) hp col

updateAllZ :: [Zombie] -> Float -> [LawnMower] -> [Zombie]
updateAllZ zombies time mowers = do
    let updated = map (`updateZombie` time) zombies
    let activeMowers = filter isActive mowers
    filter (\z -> all (\m -> not (isActive m) || not (C.checkCollision z m)) activeMowers) updated

hitZombie :: Zombie -> Int -> Zombie
hitZombie (Zombie pos hp col) damage = Zombie pos (max 0 (hp - damage)) col

clearDead :: [Zombie] -> [Zombie]
clearDead = filter ((>0) . zombieHealth)

checkFinish :: [Zombie] -> Float -> Bool
checkFinish zombies edge = any isAtEdge zombies
    where
        isAtEdge (Zombie (Position x _ _ _ _) _ _) = x <= edge

checkLawnMowerCollision :: [LawnMower] -> Zombie -> Zombie
checkLawnMowerCollision mowers zombie
    | any (\m -> isActive m && C.checkCollision zombie m) mowers =
        zombie { zombieHealth = 0 }
    | otherwise = zombie