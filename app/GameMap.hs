module GameMap (generateMap) where

import Graphics.Gloss

-- | Displays uncompressed 24/32 bit BMP images.
generateMap :: IO Picture
generateMap = do
    Bitmap bmpData <- loadBMP "img/Frontyard.bmp"
    return (Bitmap bmpData)
