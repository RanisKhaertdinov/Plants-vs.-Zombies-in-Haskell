module Plant where

import Graphics.Gloss
 


data Plant  -- (dx, dy) health
    = Plant PlantType (Float, Float) Int
    deriving (Show)

data PlantType
    = Sunflower
    | Peashooter
    | WallNut
    deriving (Eq, Show)
    
data PlantCard = PlantCard
    { cardType :: PlantType
    , cost :: Int
    , cooldown :: Int
    }

availableCards :: [PlantCard]
availableCards =
    [ PlantCard Sunflower 50 7
    , PlantCard Peashooter 100 5
    , PlantCard WallNut 50 20
    ]


prettySunflower :: Picture
prettySunflower = Pictures
  [ -- Лепестки
    Pictures [ Translate (petalX i) (petalY i) $ Color (makeColorI 255 215 0 255) $ circleSolid 15 | i <- [0..11] ]
  , -- Центр
    Color (makeColorI 139 69 19 255) $ circleSolid 13
  , -- Светлый центр
    Color (makeColorI 255 220 120 120) $ circleSolid 20
  , -- Стебель
    Color (makeColorI 34 139 34 255) $ Translate 0 (-40) $ rectangleSolid 6 36
  , -- Листья
    Color (makeColorI 60 180 60 180) $ Pictures
        [ Translate (-8) (-38) $ rotateLeaf 30
        , Translate (8) (-38) $ rotateLeaf (-30)
        ]
  ]
  where
    petalX i = 21 * cos (2 * pi * fromIntegral i / 12)
    petalY i = 21 * sin (2 * pi * fromIntegral i / 12)
    rotateLeaf a = Rotate a $ Scale 1 0.5 $ circleSolid 7

prettyPeashooter :: Picture
prettyPeashooter = Pictures
  [ -- Стебель
    Color (makeColorI 34 139 34 255) $ Translate 0 (-32) $ rectangleSolid 8 56
  , -- Лист
    Color (makeColorI 60 180 60 180) $ Translate (-14) (-45) $ Rotate 30 $ Scale 1 0.5 $ circleSolid 12
  , -- Голова
    Color (makeColorI 90 200 90 255) $ Translate 0 0 $ circleSolid 25
  , -- Рот (отверстие для гороха)
    Color (makeColorI 60 120 60 255) $ Translate 25 0 $ Scale 1 0.5 $ circleSolid 8
  , -- Глаз
    Color black $ Translate 12 10 $ circleSolid 4
  ]

prettyWallNut :: Picture
prettyWallNut = Pictures
  [ -- Тело
    Color (makeColorI 180 120 40 255) $ Scale 1 1.3 $ circleSolid 25
  , -- Блик
    Color (makeColorI 255 230 120 100) $ Translate (-10) 20 $ Scale 0.7 0.3 $ circleSolid 16
  , -- Левый глаз
    Color black $ Translate (-8) 14 $ Scale 1 1.2 $ circleSolid 4
  , -- Правый глаз
    Color black $ Translate (8) 14 $ Scale 1 1.2 $ circleSolid 4
  ]

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




