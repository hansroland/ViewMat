module Main where

import GTKWrapper
import ViewMat

import Data.Word

import Graphics.UI.Gtk

main = do
   initGUI
   ibuf <- ib_new (640, 480)
   mapM_ (\p -> ib_write_pixel ibuf p (colour p)) (ib_coords ibuf)
   ib_display ibuf "Hello"
   -- Display window
   mainGUI

colour :: (Coord, Coord) -> (Word8, Word8, Word8)
colour (x, y) = (floor $ fromIntegral x / 640 * 255, floor $ fromIntegral y / 480 * 255, 0)


coordValues n dx = [n*dx .. ((n+1)*dx - 1)]


colors :: [(RGBColor)]
colors = [(0,0,0), (255,255,255), (255,0,0), (0,255, 0), (0,0,255)]


{-

  -- Need a data type with data for Matrix maxX maxY dx dy etc
paintCell :: Int -> Int -> ImageBuffer -> RGBColor -> IO()
paintCell i j buf color =
    let point = [(x,y) | x = 
    mapM_ 
-}
