module LawnMower where

import Graphics.Gloss
import GameTypes (Position(..))

data LawnMower = LawnMower
  { lawnPos :: Float
  , lawnLane :: Float
  , isActive :: Bool
  } deriving (Show)

initialLawnMowers :: [LawnMower]
initialLawnMowers = [ LawnMower (-350) (fromIntegral i) False | i <- [0..4] ]

updateMowers :: Float -> [LawnMower] -> [LawnMower]
updateMowers dt mowers = map (updateMower dt) mowers
  where
    updateMower dt m@(LawnMower pos lane active)
      | active    = LawnMower (pos + 800 * dt) lane active
      | otherwise = m

activateMower :: Int -> [LawnMower] -> [LawnMower]
activateMower n mowers = take n mowers ++ [m { isActive = True } | m <- [mowers !! n]] ++ drop (n + 1) mowers

renderLawnMower :: Float -> LawnMower -> Picture
renderLawnMower gameTime m =
  let x = lawnPos m
      y = lawnLane m * 66.6 - 2 * 66.6
      bodyColor = if isActive m then makeColor 0.8 0.8 0.8 1.0 else makeColor 0.5 0.5 0.5 1.0  -- Brighter gray when active
      wheelColor = makeColor 0.1 0.1 0.1 1.0  -- Dark gray for wheels
      handleColor = makeColor 0.0 0.5 0.0 1.0  -- Green handle
      -- Body: Rectangle
      body = Color bodyColor $ rectangleSolid 40 20
      -- Wheels: Two circles
      wheel1 = Color wheelColor $ Translate (-10) (-8) $ circleSolid 5
      wheel2 = Color wheelColor $ Translate 10 (-8) $ circleSolid 5
      -- Handle: Thin rectangle or line, extending backward
      handle = Color handleColor $ Translate (-25) 5 $ rectangleSolid 10 3
      -- Blade effect when active: Two rotating triangles
      blade = if isActive m
              then let angle = gameTime * 360  -- Rotate 360 degrees per second
                   in Color (makeColor 0.3 0.3 0.3 1.0) $ Rotate angle $ Pictures
                        [ Translate 0 (-5) $ polygon [(0, 0), (5, 5), (-5, 5)]  -- Triangle 1
                        , Translate 0 (-5) $ polygon [(0, 0), (5, -5), (-5, -5)]  -- Triangle 2
                        ]
              else Blank
  in Translate x y $ Pictures [body, wheel1, wheel2, handle, blade]