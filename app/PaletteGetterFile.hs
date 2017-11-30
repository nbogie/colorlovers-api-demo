module PaletteGetterFile (getRandomPaletteList) where
import System.Random

import Palette hiding (main)

getRandomPaletteList :: IO (Either String PaletteList)
getRandomPaletteList  = do
  f <- getStdRandom (pickOne files)
  putStrLn $ "reading palette file" ++ f
  getPaletteList f

pickOne :: [a] -> StdGen -> (a, StdGen)
pickOne xs gen = (xs !! i, g')
  where (i, g') = randomR (0, length xs - 1) gen

getPaletteList :: FilePath -> IO (Either String PaletteList)
getPaletteList fname = do
  c <- readFile fname
  case jsonStringToPalette c of
    Right pl -> return (Right pl)
    Left e -> return (Left $ "Error parsing read json: " ++ e)

files :: [String]
files = [ "inputs/ninja.json"
        , "inputs/faves.json"
        , "inputs/many.json"
        , "inputs/candy.json"
        , "inputs/example.json"
        , "inputs/scifi.json"
        ]

