module Main where

import Graphics.Webcam.Linux
import Control.Monad (forM)
import Data.Array.Repa
import Data.Array.Repa.Repr.ForeignPtr
-- import Control.Monad.IO.Class
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Animate


produceFrame :: CamState -> Float -> IO Picture
produceFrame s t = do
  e <- runCamWith s $ do
    (w, h) <- getSize
    fimg <- grabF rgbaToAbgr
    let fp = toForeignPtr fimg
    let bf = BitmapFormat BottomToTop PxABGR
    pure $ bitmapOfForeignPtr w h bf fp False
  either (const $ error "ERROR!") pure e
                    

-- main = runCam (forM [1..100] (const grab) >> grab >>= saveBmp "test.bmp") (Webcam 0)
main = do
  a <- runCam (Webcam 0) $ do
       setSize (300,300)
       grab >>= saveBmp "1.bmp"
       grab >>= saveBmp "2.bmp"
       grab >>= saveBmp "3.bmp"
       (w, h) <- getSize
       st <- getState
       liftIO $ animateIO (InWindow "Hello, World!" (w,h) (10,10)) (makeColor 1 1 1 1) (produceFrame st) (const $ pure ())
  putStrLn $ show a
