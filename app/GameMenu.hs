module GameMenu where

import Graphics.Gloss
import GameTypes

type Label = String
type Diff = Int
type XBorder = (Float, Float)
type YBorder = (Float, Float)
data Button = Button XBorder YBorder Diff Coloring Label

renderMenu :: Picture
renderMenu = Pictures ([name]++renderButtons)

renderButtons :: [Picture]
renderButtons = map renderButton allButtons

renderButton :: Button -> Picture
renderButton (Button (l, r) (b, t) _ (Coloring rd grn blu a) text) 
    = Pictures [
        Translate (l+(r-l)/2) (b+(t-b)/2) (Color (makeColor rd grn blu a) $ rectangleSolid (r-l) (t-b))
        , Translate (l) (b+(t-b)/2) (Color (makeColor 0 0 0 1) $ Scale 0.15 0.15 $ Text text)
        ]

name :: Picture
name = Color white $ Translate (-250) 100 $ Scale 0.5 0.5 $ Text "zombies v. plants"


allButtons :: [Button]
allButtons = [
    Button (-50, 50) (30, 70) 1 (Coloring 0 1 0 1) "Easy"
    , Button (-50, 50) (-20, 20) 2 (Coloring 1 1 0 1) "Mid"
    , Button (-50, 50) (-70, -30) 3 (Coloring 1 0 0 1) "Hard"
    , Button (-50, 50) (-120, -80) 4 (Coloring 0.5 0 0 1) "B O S S"
    ]

clickButton :: (Float, Float) -> Button -> Bool
clickButton (x,y) (Button (l, r) (b, t) _ _ _) = (l < x && x < r) && b < y && y < t

getDifficulty :: (Float, Float) -> Int
getDifficulty pos = getDiff pos allButtons

getDiff :: (Float, Float) -> [Button] -> Int
getDiff _ [] = 0
getDiff point (b@(Button _ _ d _ _):bs)
    | clickButton point b = d
    | otherwise           = getDiff point bs