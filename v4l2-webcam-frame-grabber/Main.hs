{-# LANGUAGE DeriveDataTypeable #-}

module Main (
  main
) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, when)
import Data.Word (Word8)
import Foreign (Ptr)
import Foreign.Ptr (plusPtr)
import Foreign.Storable (peek)
import GHC.IO.Exception (IOErrorType(Interrupted), ioe_type)
import Graphics.V4L2
import System.Console.CmdArgs
import System.Directory (renameFile)
import System.Exit (exitFailure)
import System.IO.Error (tryIOError)
import System.IO (hPutStrLn, stderr)

import qualified Codec.Picture as CP
import qualified Codec.Picture.Png as Png
import qualified Data.Vector.Storable as SV


data CmdLineOpts =
  CmdLineOpts { videoDevice:: FilePath
              , outPng:: FilePath
              , delaySeconds :: Int
              , verbose:: Bool }
    deriving (Show, Data, Typeable)

cmdLineOpts :: CmdLineOpts
cmdLineOpts =
  CmdLineOpts { videoDevice = "/dev/video0" &= help "/dev/video0"
              , outPng = "frame.png" &= help "frame.png"
              , delaySeconds = 5 &= help "5 :: delay (in seconds) before grabbing next frame"
              , verbose = False &= explicit &= name "verbose" &= help "run in a verbose mode"
              } &=
                program "v4l2-webcam-frame-grabber" &=
                help "Grabs a video frame and saves it to a png file every delayseconds seconds"

main :: IO ()
main = do
  opts <- cmdArgs cmdLineOpts :: IO CmdLineOpts
  let delayMicros = (delaySeconds opts) * 1000000

  forM_ [(0 :: Int) ..] $ \i -> do
    grabFrame opts i
    threadDelay delayMicros

grabFrame :: CmdLineOpts -> Int -> IO ()
grabFrame opts i = do
  let tmpFile = (outPng opts) ++ ".tmp"
  e <- tryIOError $ withDevice (videoDevice opts) $ \d -> do
    f <- setFormat d Capture . (\f->f{ imagePixelFormat = PixelRGB24 }) =<< getFormat d Capture
    checkFormat f

    when (verbose opts) $ do
      info $ "Frame number (" ++ (show i) ++ ") size: " ++ show (imageWidth f) ++ "x" ++ show (imageHeight f) ++ " pixels (" ++ show (imageSize f) ++ " bytes)"

    withFrame d f $ \p n -> do
      if n == imageSize f
        then do
          img <- toImage (imageWidth f) (imageHeight f) p
          Png.writePng tmpFile img
          renameFile tmpFile (outPng opts)

        else warn $ "Incomplete frame (" ++ show n ++ " bytes, expected " ++ show (imageSize f) ++ " bytes)"

  case e of
    Left f | ioe_type f == Interrupted -> return ()
           | otherwise -> ioError f
    Right () -> return ()

toImage :: Int -> Int -> Ptr Word8 -> IO (CP.Image CP.PixelRGB8)
toImage w h p = do
  pixels <- SV.generateM (w*h*3) (peek . plusPtr p)
  return $ CP.Image w h pixels

checkFormat :: ImageFormat -> IO ()
checkFormat f = do
  when (imagePixelFormat f /= PixelRGB24) $ err "could not set RGB24 pixel format"
  when (imageBytesPerLine f /= imageWidth f * 3) $ err "cannot handle extra padding"
  when (imageSize f /= imageBytesPerLine f * imageHeight f) $ err "cannot handle image size"

err :: String -> IO a
err msg = (hPutStrLn stderr $ "**ERROR: [v4l2-webcam-frame-grabber] " ++ msg) >> exitFailure

warn :: String -> IO ()
warn msg = hPutStrLn stderr $ "++ WARN: [v4l2-webcam-frame-grabber] " ++ msg

info :: String -> IO ()
info msg = hPutStrLn stderr $ "   INFO: [v4l2-webcam-frame-grabber] " ++ msg
