module Args where

import Data.List
import Result
import Text.Read (readMaybe)

data Args = Args
  { argImageConfigFile :: Maybe String, -- ^ Given with the `-imageConfigFile` argument
    argSceneFile :: Maybe String, -- ^ Given with the `-sceneFile` argument
    argOutFile :: Maybe String, -- ^ Given with the `-outFile` argument
    argNrSamples :: Maybe Int -- ^ Given with the `-imageNrSamples` argument
  }
  deriving (Eq, Show)

data ParseArgsError = InvalidArgs deriving (Eq, Show)

type ArgMap = [(String, String)]

-- >>> toArgMap ["-x", "y"]
-- Success [("x","y")]
--
-- >>> toArgMap ["-x", "y", "-a", "b"]
-- Success [("x","y"),("a","b")]
--
-- >>> toArgMap ["x", "y"]
-- Error InvalidArgs
--
-- >>> toArgMap ["-x", "y", "-z"]
-- Error InvalidArgs
toArgMap :: [String] -> Result ParseArgsError ArgMap
toArgMap [] = Success []
toArgMap (x:y:xs)
    | head x == '-' = let rest = toArgMap xs in
                      case rest of
                        Success r -> Success ((tail x, y):r)
                        Error _ -> Error InvalidArgs
    | otherwise = Error InvalidArgs
toArgMap _ = Error InvalidArgs

-- >>> getArg "key" [("key", "value")]
-- Just "value"
getArg :: String -> ArgMap -> Maybe String
getArg key argMap = snd <$> find (\(k, _) -> k == key) argMap

-- >>> readArg "name" [("name", "1")] :: Maybe Int
-- Just 1
--
-- >>> readArg "name" [("name", "one")] :: Maybe Int
-- Nothing
--
-- >>> readArg "number" [("name", "1")] :: Maybe Int
-- Nothing
readArg :: (Read a) => String -> ArgMap -> Maybe a
readArg key argMap = find (\(k, _) -> k == key) argMap >>= readMaybe . snd

-- >>> procArgs ["-imageNrSamples", "200", "-outFile", "image.bmp"]
-- Success (Args {argImageConfigFile = Nothing, argSceneFile = Nothing, argOutFile = Just "image.bmp", argNrSamples = Just 200})
procArgs :: [String] -> Result ParseArgsError Args
procArgs args =
    case toArgMap args of
      Success argMap1 -> Success $ Args argImageConfigFile argSceneFile argOutFile argNrSamples
        where
          argImageConfigFile = getArg "-imageConfigFile" argMap1
          argSceneFile = getArg "-sceneFile" argMap1
          argOutFile = getArg "-outFile" argMap1
          argNrSamples = readArg "-imageNrSamples" argMap1
      Error _ -> Error InvalidArgs
      

 