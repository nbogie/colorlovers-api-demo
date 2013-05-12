module PaletteGetterWeb where

import HttpUtils (getBody)
import Json hiding (main)


-- Sample api requests
-- by keyword (likely more than one):
-- http://www.colourlovers.com/api/palettes?keywords=scifi&keywordExact=1&format=json
--
-- One, random: 
-- http://www.colourlovers.com/api/palettes/random?format=json

getRandomPaletteList :: IO (Either String PaletteList)
getRandomPaletteList = do
  bdy <- getBody "http://www.colourlovers.com/api/palettes/random?format=json"
  return (bdy >>= jsonStringToPalette)

main = do
  pE <- getRandomPaletteList
  case pE of
    Left err -> putStrLn $ "Error getting palette: "++ err
    Right p -> print p

-- http://www.colourlovers.com/palette/1891438/Sci_Fi_Fluid
