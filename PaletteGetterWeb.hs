module PaletteGetterWeb where

import HttpUtils (getBody)
import Json hiding (main)

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

main = do
  pE <- getRandomPaletteList
  case pE of
    Left err -> putStrLn $ "Error getting palette: "++ err
    Right p -> print p
