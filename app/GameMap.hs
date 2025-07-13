module GameMap where

import Graphics.Gloss

generateMap :: IO Picture
generateMap = do
    Bitmap bmpData <- loadBMP "img/Frontyard.bmp"
    return $ Translate 0 0 $ Bitmap bmpData