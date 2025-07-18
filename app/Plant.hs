module Plant where

import Graphics.Gloss

-- | Represents a plant on the field, with its type, position, and health.
data Plant = Plant PlantType (Float, Float) Float
    deriving (Show)

-- | Types of plants available in the game.
data PlantType = Sunflower | Peashooter | WallNut
    deriving (Eq, Show)

-- | Card data for plant selection (type, cost, cooldown).
data PlantCard = PlantCard
    { cardType :: PlantType  -- ^ The type of plant this card represents
    , cost :: Int            -- ^ Sun cost to plant
    , cooldown :: Int        -- ^ Cooldown time in seconds
    }

-- | List of all available plant cards.
availableCards :: [PlantCard]
availableCards =
    [ PlantCard Sunflower 50 7
    , PlantCard Peashooter 100 5
    , PlantCard WallNut 50 20
    ]

-- | Render a sunflower as a Gloss picture.
prettySunflower :: Picture
prettySunflower = Translate 0 (10) $ Pictures
  [ -- Petals
    Pictures [ Translate (petalX i) (petalY i) $ Color (makeColorI 255 215 0 255) $ circleSolid 10 | i <- [0..11] ]
  , -- Center
    Color (makeColorI 139 69 19 255) $ circleSolid 13
  , -- Highlighted center
    Color (makeColorI 255 220 120 120) $ circleSolid 20
  ]
  where
    petalX i = 21 * cos (2 * pi * fromIntegral i / 12)
    petalY i = 21 * sin (2 * pi * fromIntegral i / 12)
  
prettySunflowerStem :: Picture
prettySunflowerStem = Translate 0 (10) $ Pictures
  [ -- Stem
    Color (makeColorI 34 139 34 255) $ Translate 0 (-40) $ rectangleSolid 6 16
  , -- Leaves
    Color (makeColorI 60 180 60 180) $ Pictures
        [ Translate (-8) (-38) $ rotateLeaf 30
        , Translate (8) (-38) $ rotateLeaf (-30)
        ]
  ]
  where
    rotateLeaf a = Rotate a $ Scale 1 0.5 $ circleSolid 7

-- | Render a peashooter as a Gloss picture.
prettyPeashooter :: Picture
prettyPeashooter = Translate 0 0 $ Pictures
  [ -- Head
    Color (makeColorI 90 200 90 255) $ Translate 0 0 $ circleSolid 25
  , -- Mouth (pea hole)
    Color (makeColorI 60 120 60 255) $ Translate 25 0 $ Scale 1 0.5 $ circleSolid 8
  , -- Eye
    Color black $ Translate 12 10 $ circleSolid 4
  ]
prettyPeashooterStem :: Picture
prettyPeashooterStem = Translate 0 (10) $ Pictures
  [ -- Stem
    Color (makeColorI 34 139 34 255) $ Translate 0 (-32) $ rectangleSolid 8 56
  , -- Leaf
    Color (makeColorI 60 180 60 180) $ Translate (-14) (-45) $ Rotate 30 $ Scale 1 0.5 $ circleSolid 12
  ]

-- | Render a wall-nut as a Gloss picture.
prettyWallNut :: Picture
prettyWallNut = Pictures
  [ -- Body
    Color (makeColorI 180 120 40 255) $ Scale 1 1.3 $ circleSolid 25
  , -- Highlight
    Color (makeColorI 255 230 120 100) $ Translate (-10) 20 $ Scale 0.7 0.3 $ circleSolid 16
  , -- Left eye
    Color black $ Translate (-8) 14 $ Scale 1 1.2 $ circleSolid 4
  , -- Right eye
    Color black $ Translate (8) 14 $ Scale 1 1.2 $ circleSolid 4
  ]

generateStem :: Plant -> Picture
generateStem (Plant Sunflower (x, y) health) = 
    if health > 0 
    then Translate x y prettySunflowerStem
    else blank
generateStem (Plant Peashooter (x, y) health) = 
    if health > 0 
    then Translate x y prettyPeashooterStem
    else blank
generateStem (Plant WallNut (x, y) health) = blank


-- | Render a plant based on its type and health.
generatePlant :: Plant -> Picture 
generatePlant (Plant Sunflower (x, y) health) = 
    if health > 0 
    then Translate x y prettySunflower
    else blank
generatePlant (Plant Peashooter (x, y) health) = 
    if health > 0 
    then Translate x y prettyPeashooter
    else blank
generatePlant (Plant WallNut (x, y) health) =
    if health > 0
    then Translate x y prettyWallNut
    else blank

generateFullPlants :: [Plant] -> Picture
generateFullPlants [] = blank
generateFullPlants (x:xs) = Pictures ([generateStem x] ++ [(generateFullPlants xs)] ++ [generatePlant x])
