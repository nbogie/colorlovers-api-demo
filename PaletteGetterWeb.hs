module PaletteGetterWeb where
import System.Environment (getArgs)
import HttpUtils (getBody)
import Palette hiding (main)

-- Sample api requests
--
-- List exactly matching keyword(s) (likely more than one):
-- http://www.colourlovers.com/api/palettes?keywords=scifi&keywordExact=1&format=json
--
-- One specific
-- http://www.colourlovers.com/api/palette/1891438/Sci_Fi_Fluid?format=json
--
-- One, random: 
-- http://www.colourlovers.com/api/palettes/random?format=json
getRandomPaletteList :: IO (Either String PaletteList)
getRandomPaletteList = do
  bdy <- getBody "http://www.colourlovers.com/api/palettes/random?format=json&showPaletteWidths=1"
  return (bdy >>= jsonStringToPalette)

getPaletteById :: Integer -> IO (Either String PaletteList)
getPaletteById pid = do
  bdy <- getPaletteByIdJson pid
  return (bdy >>= jsonStringToPalette)

getPaletteByIdJson ::  Integer -> IO (Either String String)
getPaletteByIdJson pid =  do
  getBody ("http://www.colourlovers.com/api/palette/"++ show pid ++ "?format=json&showPaletteWidths=1") 

-- take a list of palette ids and download them as json to stdout.  Otherwise, get one, randomly.
main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> do
       pE <- getRandomPaletteList
       case pE of
          Left err -> error $ "Error getting palette: "++ err
          Right p -> print p
    pidStrs -> do 
        let pids = (map read pidStrs)::[Integer]
        psE <- fmap sequence $ mapM getPaletteByIdJson pids
        case psE of
          Right ps -> mapM_ putStrLn ps
          Left err -> error $ "Error getting palette: "++ err
