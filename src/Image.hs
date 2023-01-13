module Image where

import Parser
import Result
import Scene.Loader
import Test.SimpleTest.Mock (TestableMonadIO (..))
import Prelude hiding (readFile)

data Image = Image
  { imageAspectRatio :: Double,
    imageWidth :: Int,
    imageHeight :: Int,
    imageNrSamples :: Int,
    imageMaxDepth :: Int
  }
  deriving (Eq, Show)

setupImage :: Int -> Int -> Int -> Int -> Image
setupImage w h nrSamples maxDepth =
  Image
    { imageAspectRatio = fromIntegral w / fromIntegral h,
      imageWidth = w,
      imageHeight = h,
      imageNrSamples = nrSamples,
      imageMaxDepth = maxDepth
    }

defaultImage :: Image
defaultImage = setupImage 800 450 100 50

-- >>> runParser imageParser "image {width 800 height 450 nr_samples 100 max_depth 50}"
-- Success (Image {imageAspectRatio = 1.7777777777777777, imageWidth = 800, imageHeight = 450, imageNrSamples = 100, imageMaxDepth = 50},"")
imageParser :: Parser Image
imageParser = many lower `pThen` ws `pThen` dict4Parser setupImage
              ("width", number)
              ("height", number)
              ("nr_samples", number)
              ("max_depth", number)

getImageConfig :: TestableMonadIO io => Maybe String -> io (Result LoadingError Image)
getImageConfig Nothing = return $ Success defaultImage
getImageConfig (Just filePath) = loadImageConfig filePath

loadImageConfig :: TestableMonadIO io => String -> io (Result LoadingError Image)
loadImageConfig path = do
  fileExists <- liftIO $ doesFileExist path
  if fileExists then do
    fileContent <- liftIO $ readFile path
    case runParser imageParser fileContent of
      Success (img, _) -> return $ Success img
      Error e -> return $ Error $ ParseFailed e
  else return $ Error FileNotFound
  where liftIO = id