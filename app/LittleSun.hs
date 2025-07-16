module LittleSun where

import Graphics.Gloss
import Plant
import GameStates

-- | Create a new sun object falling from start to end at time t.
createSun :: (Float, Float) -> (Float, Float) -> Float -> Sun
createSun start end t = Sun start end 25 t 0

-- | Render a sun as a Gloss picture.
prettySun :: Picture
prettySun = Pictures
  [ Pictures [ Translate (petalX i) (petalY i) $ Color (makeColorI 255 215 0 255) $ circleSolid 12 | i <- [0..11] ]
  , Color (makeColorI 255 255 100 255) $ circleSolid 9
  , Color (makeColorI 255 255 180 120) $ circleSolid 20
  ]
  where
    petalX i = 16 * cos (2 * pi * fromIntegral i / 12)
    petalY i = 16 * sin (2 * pi * fromIntegral i / 12)

-- | Render a sun at its current falling position.
renderSun :: Sun -> Picture
renderSun (Sun (x0, y0) (x1, y1) _ _ f)
    | f >= 1 = Translate x1 y1 prettySun
    | otherwise = Translate (x0 + (x1 - x0) * f) (y0 + (y1 - y0)*f) prettySun

-- | Returns True if the given coordinates are within the clickable area of the sun.
isSunClicked :: Sun -> (Float, Float) -> Bool
isSunClicked (Sun (x0, y0) (x1, y1) _ _ f) (x', y') =
    let (x, y) = if f >= 1 then (x1, y1) else (x0 + (x1 - x0) * f, y0 + (y1 - y0) * f)
    in abs (x - x') < 20 && abs (y - y') < 20

-- | Update all suns, advancing their fall and removing those that have expired.
updateSuns :: Float -> [Sun] -> [Sun]
updateSuns dt = filter (\s -> dt - bornTime s < 8) . map (\s -> s { fallProgress = min 1 (fallProgress s + dt * 0.3)})

-- | Generate new suns from sunflowers that are ready to produce them.
generateSun :: [(Plant, Float)] -> Float -> [Sun] -> [Sun]
generateSun plants now suns =
    [ createSun (x, y + 70) (x, y) now
    | (Plant Sunflower (x, y) _, lastTime) <- plants
    , now - lastTime >= 10
    , not (any (\s -> abs (fst (endPos s) - x) < 1 && abs (snd (endPos s) - y) < 1 && fallProgress s < 1) suns)
    ]