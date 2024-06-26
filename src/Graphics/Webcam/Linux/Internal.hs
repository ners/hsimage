{-# LANGUAGE GeneralizedNewtypeDeriving, 
  TypeSynonymInstances, 
  FlexibleInstances,
  FlexibleContexts,
  MultiParamTypeClasses, 
  ExistentialQuantification #-}

module Graphics.Webcam.Linux.Internal
       ( 
         -- * Data Types
         Image
       , Webcam (..)
         -- * V4L Monad
       , V4lCamT (..)
       , InnerMonad (..)
       , Control.Monad.Trans.liftIO
       -- ** Running the Monad
       , runCam
       , runCamWith
       -- ** Functions in the V4L Monad
       , getState
       , setState
       , openCam
       , closeCam
       , getImageFormat
       , setSize
       , getSize
       , grab
       , grabF
       , getDev
       , saveBmp
       , findImageFormat
       -- * Other useful functions
       , camToName
       , rgbaToAbgr
       , flipY
       , chooseSize
       , frameToRGBA
       , frameToRGBAF
       , CamState (..)
       ) where

import Graphics.V4L2
import qualified Data.Set as S (toList)
import Data.List (sortBy,minimumBy)
import Data.Word
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Trans
import Control.Exception

import Foreign.Ptr
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Marshal.Array

import Data.Array.Repa hiding ((++))
import Data.Array.Repa.Eval
import Data.Array.Repa.Repr.ForeignPtr
import Data.Array.Repa.Repr.ByteString
import Data.Vector.Unboxed.Base
import Data.ByteString (pack)

import Prelude hiding (traverse)

import qualified Codec.BMP as BMP


{-| An image data type. This uses the Array type Data.Array.Repa.Array. -}
type Image r a = Array r DIM3 a


data Webcam = Webcam Int


data CamState = CamState { 
  camstateCam    :: Webcam,
  camstateDev    :: Maybe Device,
  camstateFormat :: Maybe ImageFormat
  }
                             

newtype V4lCamT m a = V4lCamT { unV4lCam :: InnerMonad m a }
    deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadIO)


type InnerMonad m a = ErrorT String (StateT CamState m) a


instance MonadTrans V4lCamT where
  lift = V4lCamT . lift . lift


instance Monad m => MonadError String (V4lCamT m) where
    throwError = V4lCamT . throwError
    catchError (V4lCamT act) errf = V4lCamT $ catchError act (unV4lCam . errf)


{-| Get the state data. Internal. -}
getState :: Monad m => V4lCamT m CamState
getState = V4lCamT $ lift get


{-| Set the state data. Internal. -}
setState :: Monad m => CamState -> V4lCamT m ()
setState = V4lCamT . lift . put


{-| Get the device. Internal. -}
getDev :: Monad m => V4lCamT m Device
getDev = camstateDev `fmap` getState >>= maybe (throwError "Device not open.") return


{-| Save the given 'Image' as BMP in the file with the given name.
This function currently takes a detour via lists when converting the image to a 'ByteString',
and is therefore probably slower than necessary (FIXME). -}
saveBmp :: (MonadIO m, Source r Word8) => FilePath -> Image r Word8 -> V4lCamT m ()
saveBmp name i = do
  -- This does not compile; why not? Fill instances missing in Repa?
  -- a <- liftIO $ ((copyP i :: IO (Image B Word8)) >>=
  --                return . toByteString >>= 
  --                return . BMP.packRGBA32ToBMP w h >>=
  --                BMP.writeBMP name >> return True) `onException` return False

  a <- liftIO $ (let bs = pack (toList i); bsa = BMP.packRGBA32ToBMP w h bs
                 in 
                  BMP.writeBMP name bsa >> return True) `onException` return False
  if not a 
    then throwError ("Could not save image " ++ name)
    else return ()
  where (Z :. h :. w :. k) = extent i
                 

{-| Convert the given 'Webcam' to a name (of a device that can be opened). -}
camToName :: Webcam -> String
camToName (Webcam n) = "/dev/video" ++ show n


{-| Open the given 'Webcam'. -}
openCam :: MonadIO m => Webcam -> V4lCamT m ()
openCam w = do
  let name = camToName w
  mdev <- liftIO $ Just `fmap` openDevice name `onException` return Nothing
  case mdev of
    Just dev -> V4lCamT $ lift $ modify $ \s -> s { camstateDev = Just dev }
    _        -> throwError $ "Could not open device " ++ name


{-| Closes the currently open 'Webcam'. -}
closeCam :: MonadIO m => V4lCamT m ()
closeCam = getDev >>= liftIO . closeDevice


{-| Given a 'Webcam', runs a 'V4lCamT' action with it.
All actions may 'throwError', which can be caught with 'catchError'. 
In case of an error, 'runCam' returns 'Left' with an errormessage. 
Otherwise, it returns a 'Right' with the result. 

This function sets a "reasonable" image format using 'findImageFormat'
after opening the webcam. Then, the supplied action is run and the camera
is closed again.

You may want to set a different image size or image format,
when that is supported.
-}
runCam :: MonadIO m => Webcam -> V4lCamT m a -> m (Either String a)
runCam cam act = r
    where 
      uact = unV4lCam ((openCam cam >> findImageFormat >> act >>= \a -> closeCam >> return a) 
                       `catchError` 
                       (\e -> liftIO (putStrLn "final close!") >> closeCam >> throwError e))
      eact = runErrorT uact
      r    = evalStateT eact s
      s    = CamState cam Nothing Nothing


{-| Like 'runCam', but runs with a given state. -}
runCamWith :: MonadIO m => CamState -> V4lCamT m a -> m (Either String a)
runCamWith s act = r
  where        
    uact = unV4lCam (act
                     `catchError` 
                     (\e -> liftIO (putStrLn "final close!") >> closeCam >> throwError e))
    eact = runErrorT uact
    r    = evalStateT eact s


{-| Grab a new image from the currently open camera.
May 'throwError' if something goes wrong. -}
grab :: MonadIO m => V4lCamT m (Image U Word8)
grab = do 
  dev <- getDev
  format <- getImageFormat
  liftIO $ withFrame dev format (\p i -> frameToRGBA format p >>= computeP)
  -- runIL (writeImage "testimage.jpg" img)


{-| Like 'grab', but applies the given function to the captured image. -}
grabF :: MonadIO m => (Image D Word8 -> Image D Word8) -> V4lCamT m (Image F Word8)
grabF conv = do 
  dev <- getDev
  format <- getImageFormat
  (w,h) <- getSize
  liftIO $ withFrame dev format $ \p i -> do
    f <- frameToRGBAF format p conv
    let n = w * h * 4
    fa <- mallocForeignPtrArray n
    computeIntoP fa f -- (conv f)
    return $ fromForeignPtr (Z :. h :. w :. 4) fa
  -- runIL (writeImage "testimage.jpg" img)
{- INLINE grabF -}
    

{-| Get the currently used image format.
May 'throwError' if the format has not been set. -}
getImageFormat :: Monad m => V4lCamT m ImageFormat
getImageFormat = camstateFormat `fmap` getState >>= maybe (throwError "Format is not set.") return


{-| This function sets the size to the next fitting size the
connected web camera supports. You can query the actual size with 'getSize'. -}
setSize :: MonadIO m => (Int, Int) -> V4lCamT m ()
setSize (w,h) = do
  szs <- getPossibleSizes
  let (w',h') = chooseClosestSize (w,h) szs
  fmt <- getImageFormat
  let fmt' = fmt { imageWidth = w',
                   imageHeight = h' }
  dev <- getDev
  liftIO $ setFormat dev Capture fmt'
  format <- liftIO $ getFormat dev Capture
  getState >>= \s -> setState (s { camstateFormat = Just format } )
  return ()

{-| Returns the image with and height of the images captured by
the currently open web cam. -}
getSize :: Monad m => V4lCamT m (Int, Int)
getSize = getImageFormat >>= \f -> return (imageWidth f, imageHeight f)
  


--------------------------------------------------------------------------------

{-| Choose a "suitable" size from a bunch of frame sizes. -}
chooseSize :: FrameSizes -> Maybe FrameSize
chooseSize (DiscreteSizes s) = let l = reverse $ sortBy (\a b -> compare (frameWidth a) (frameWidth b)) $ S.toList s
                         in maybeMedian l
chooseSize (StepwiseSizes minW maxW stepW minH maxH stepH) = Just $ FrameSize maxW maxH



chooseClosestSize :: (Int, Int) -> FrameSizes -> (Int, Int)
chooseClosestSize (w,h) (DiscreteSizes s) = (w',h')
  where l         = Prelude.map (\a -> (frameWidth a, frameHeight a)) $ S.toList s
        l'        = Prelude.map (\(x,y) -> (x,y,(w-x)^2 + (h-y)^2)) l
        (w',h',_) = minimumBy (\(_,_,a) (_,_,b) -> compare a b) l'

chooseClosestSize (w,h) (StepwiseSizes minW maxW stepW minH maxH stepH) = (w', h')
  where w' = min minW $ max ((w `div` stepW) * stepW) minW
        h' = min minH $ max ((h `div` stepH) * stepH) minH


maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (a:_) = Just a

maybeMedian :: [a] -> Maybe a
maybeMedian [] = Nothing
maybeMedian l = Just $ l !! n 
  where n = m - 1 - m `div` 4
        m = length l
                                    

getPossibleSizes :: MonadIO m => V4lCamT m FrameSizes
getPossibleSizes = do
  dev <- getDev
  format' <- getImageFormat
  liftIO $ queryFrameSizes dev (imagePixelFormat format')
  
  
{-| Finds an image format, sets the current device to it and sets it so it can be 
retrieved with 'getImageFormat'. 
Currently this function will set a 'PixelRGB24' format.
Add more intelligence to this if needed. -}
findImageFormat :: MonadIO m => V4lCamT m ()
findImageFormat = do
  dev <- getDev
  (info,cap) <- liftIO $ deviceInfoCapabilities dev
  -- liftIO $ putStrLn $ show cap
  liftIO $ setVideoInput dev (fromIntegral 0)
         
  sizes <- liftIO $ queryFrameSizes dev PixelRGB24
  format' <- liftIO $ getFormat dev Capture
  (FrameSize sx sy) <- let mf = chooseSize sizes
                       in case mf of
                         Just s -> return s 
                         _      -> throwError "No suitable size found!"
  let format = format' { imageWidth = sx,
                         imageHeight = sy,
                         imagePixelFormat = PixelRGB24 }
  -- liftIO $ putStrLn ("Size: " ++ show sx ++ " , " ++ show sy)
  liftIO $ setFormat dev Capture format                       
  format <- liftIO $ getFormat dev Capture
  liftIO $ putStrLn $ show format
  getState >>= \s -> setState (s { camstateFormat = Just format } )


{-| Flips the Y axis of a given image. -}
flipY :: Source r a => Image r a -> Image D a
flipY i = backpermute sh (\(Z :. y :. x :. j) -> Z :. h - 1 - y :. x :. j) i
  where sh@(Z :. h :. w :. k) = extent i


rgbaToAbgr :: Source r a => Image r a -> Image D a
rgbaToAbgr i = backpermute sh (\(Z :. y :. x :. j) -> Z :. y :. x :. (k - 1 - j)) i
  where sh@(Z :. h :. w :. k) = extent i
{-# INLINE rgbaToAbgr #-}

{- FIXME: To make things faster, put swapping RGBA directly in here. -}
{-| Converts a RGB image from the camera to an RGBA image that can e.g. be
stored as BMP with 'saveBMP'. -}
rgbToRgba :: Num a => Source r a => Image r a -> Image D a
rgbToRgba src | k == 3 = flipY $ traverse src shf f
              | k == 4 = delay src
              | otherwise = error "Could not convert image to rgba"
  where shf _                                = (Z :. h :. w :. 4)
        (Z :. h :. w :. k)                   = extent src
        f g p@(Z :. y :. x :. j) | j == 3     = 255
                                 | otherwise = g p


{-| Copy the image stored at the given pointer in the given format into a 4-channel RGBA image.
This currently works only for 3-channel source images which are stored contiguously.
There may be no line padding. -}
frameToRGBA :: (Data.Vector.Unboxed.Base.Unbox a, Num a, Elt a, Storable a) => ImageFormat -> Ptr a -> IO (Image D a)
frameToRGBA i p = do
  fp <- newForeignPtr_ p
  let src = fromForeignPtr sh fp 
      sh  = Z :. h :. w :. 3
      w   = imageWidth i
      h   = imageHeight i
  return $ rgbToRgba src

{-| Like 'frameToRGBA', but applies 
 > f . rgbToRgba
 to the image. -}
frameToRGBAF :: (Data.Vector.Unboxed.Base.Unbox a, Num a, Elt a, Storable a) => 
                ImageFormat                  -- ^ The image format 
                -> Ptr a                     -- ^ The image data
                -> (Image D a -> Image D a)  -- ^ The function /f/ that is applied to the data
                -> IO (Image D a)            -- ^ Returns the image.
frameToRGBAF i p f = do
  fp <- newForeignPtr_ p
  let src = fromForeignPtr sh fp 
      sh  = Z :. h :. w :. 3
      w   = imageWidth i
      h   = imageHeight i
  return $ (f . rgbToRgba) src

