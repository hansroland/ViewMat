module GTKWrapper
   where

import Data.Word
import Data.Array.IO
import Data.Array.Base (unsafeWrite)
import Graphics.UI.Gtk
import qualified Graphics.UI.Gtk.Gdk.GC as GC


-- | The type of pixel coordinates.
type Coord   = Int

-- | The type of pixel channels.
type Channel = Word8

type RGBColor = (Word8, Word8, Word8)

{- |
  An @ImageBuffer@ is a space-efficient grid of 24-bit
  RGB pixels (i.e., 8 bits per channel) that the GTK
  library can also access. (The latter gives us the
  ability to load\/save\/display the graphics, rather
  than just manipulate it in memory.)
-}
data ImageBuffer = ImageBuffer
  {
    pixbuf :: Pixbuf,
    imgWidth, imgHeight :: Coord,
    rowstride :: Coord,
    raw :: PixbufData Coord Channel
  }


{- |
  Make a new 'ImageBuffer' of the specified size. Valid
  coordinates will be in the range (0,0) to (x-1, y-1),
  with (0,0) being the top-left corner of the image.
-}
ib_new :: (Int,Int) -> IO ImageBuffer
ib_new (x,y) = do
  core <- pixbufNew ColorspaceRgb False 8 x y
  build core

{- |
  This is a quick shortcut for displaying an image
  on screen. Given an 'ImageBuffer' and a window
  title, this will display the image in a new window.
  Clicking the window's close button will call GTK's
  'mainQuit' function. For example,

  > ib_display buf "My Window Title"
  > main_loop

  will display @buf@ on screen and halt the program
  (or at least thread 0) until the user clicks the
  close button. Crude, but useful for quick testing.
-}
ib_display :: ImageBuffer -> String -> IO ()
ib_display ib title = do
  window <- windowNew
  windowSetTitle window title
  windowSetDefaultSize window (imgWidth ib) (imgHeight ib)
  canvas <- ib_canvas ib
  containerAdd window canvas
  window `onDestroy` mainQuit
  widgetShowAll window


{- |
  Returns a GTK+ canvas object which displays the graphcs
  in the 'ImageBuffer' and will automatically repaint
  itself in response to damage.
-}
ib_canvas :: ImageBuffer -> IO DrawingArea
ib_canvas ib = do
    canvas <- drawingAreaNew
    canvas `onExposeRect` (redraw canvas)
    return canvas
  where
    redraw canvas _ = do
      dc <- widgetGetDrawWindow canvas
      gc <- GC.gcNew dc
      drawPixbuf dc gc (pixbuf ib) 0 0 0 0 (-1) (-1) RgbDitherNone 0 0
      return ()


build :: Pixbuf -> IO ImageBuffer
build core = do
  sx <- pixbufGetWidth     core
  sy <- pixbufGetHeight    core
  s  <- pixbufGetRowstride core
  r  <- pixbufGetPixels    core
  return $ ImageBuffer
    {
      pixbuf = core,
      imgWidth = sx,
      imgHeight = sy,
      rowstride = s,
      raw = r
    }


{- |
  Overwrite the specified pixel with the specified colour (R, G, B).

  Note that pixel coordinates are /not/ checked, for efficiency reasons.
  However, supplying invalid pixel coordinates may result in behaviour ranging
  from corrupted graphical output to random program crashes. See
  "Graphics.EasyRaster.GTK.Paranoid" for a version of this function with
  bounds checking turned on.
-}
ib_write_pixel :: ImageBuffer -> (Coord,Coord) -> RGBColor -> IO ()
ib_write_pixel buf (x,y) (r,g,b) = do
  -- Caution: No range checks!!
  let p = y * rowstride buf + x * 3
  let rb = raw buf
  unsafeWrite rb (p + 0) r
  unsafeWrite rb (p + 1) g
  unsafeWrite rb (p + 2) b


{- |
  Return a list containing all valid coordinates for this 'ImageBuffer'.
  Useful when you want to process all pixels in the image; now you
  can just use 'mapM' or whatever.
-}

ib_coords :: ImageBuffer -> [(Coord,Coord)]
ib_coords buf = [ (x,y) | y <- [0 .. (imgHeight buf) - 1], x <- [0 .. (imgWidth buf) - 1] ]


