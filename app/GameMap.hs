module GameMap where

import Graphics.Gloss

-- | Loads the background image and returns it as a Gloss Picture.
generateMap :: IO Picture
generateMap = do
    Bitmap bmpData <- loadBMP "img/PvZFrontyard.bmp"
    return $ Translate 0 0 $ Bitmap bmpData