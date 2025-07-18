module GameMenu where

import Graphics.Gloss
import GameTypes

type Diff = Int
type XBorder = (Float, Float)
type YBorder = (Float, Float)
data Button = Button XBorder YBorder Diff Coloring

renderMenu :: Picture
renderMenu = Pictures ([]++renderButtons)

renderButtons :: [Picture]
renderButtons = map renderButton allButtons

renderButton :: Button -> Picture
renderButton (Button (l, r) (b, t) _ (Coloring rd grn blu a)) = Translate (r-l) (t-b) (Color (makeColor rd grn blu a) $ rectangleSolid (r-l) (t-b))


allButtons :: [Button]
allButtons = [
    Button (-50, 50) (-50, -200) 1 (Coloring 0 1 0 1)
    , Button (-50, 50) (-30, 70) 2 (Coloring 1 1 0 1)
    ]

clickButton :: (Float, Float) -> Button -> Bool
clickButton (x,y) (Button (l, r) (b, t) _ _) = (l < x && x < r) && b < y && y < t

getDifficulty :: (Float, Float) -> Int
getDifficulty pos = getDiff pos allButtons

getDiff :: (Float, Float) -> [Button] -> Int
getDiff _ [] = 0
getDiff point (b@(Button _ _ d _):bs)
    | clickButton point b = d
    | otherwise           = getDiff point bs