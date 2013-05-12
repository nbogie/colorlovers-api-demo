{-# LANGUAGE OverloadedStrings #-} 
module Json where
--
-- example urls for palettes
-- http://www.colourlovers.com/api/palette/1244?format=json
-- http://www.colourlovers.com/api/palettes/top?keywords=candy&showPaletteWidths=1&format=json
--
-- WARNING: 
-- This is a bad example of Aeson
--
import Control.Monad (mzero)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import Numeric (readHex)

testFile = "inputs/ninja.json" -- "inputs/many.json" -- "inputs/candy.json" -- "inputs/example.json"

main::IO ()
main = demoJson

demoJson ::  IO ()
demoJson = do
  c <- readFile testFile
  case jsonStringToPalette c of
        Right (PaletteList (p:_)) -> putStrLn $ paletteToGlossHaskellSrc p
        Right _                   -> error $ "Parsed to empty palette list! File: " ++ testFile
        Left e                    -> error $ "Error parsing json: " ++ e ++ " from file " ++ testFile

jsonStringToPalette :: String -> Either String PaletteList
jsonStringToPalette str = 
  eitherDecode (BS.pack str)

data PaletteList = PaletteList [Palette] deriving (Show)
data Palette = Palette 
  { pId :: Integer
  , pTitle::String
  , pUserName::String
  , pColors :: [(Int, Int, Int)]
  , pWidths :: Maybe [Float]
  , pUrl :: String
  }
  deriving (Show)

instance FromJSON PaletteList where
  parseJSON ar@(Array _a) = do
    -- how to take individuals from the array: 
    -- p <- parseJSON (a Data.Vector.! 0)
    ps <- parseJSON ar
    return $ PaletteList ps
  parseJSON _  = mzero

instance FromJSON Palette where
  parseJSON (Object o) = do
    i    <-  o  .:  "id"
    t    <-  o  .:  "title"
    u    <-  o  .:  "userName"
    cs   <-  o  .:  "colors"
    ws   <-  o  .:? "colorWidths"
    url  <-  o  .:  "url"
    return $ Palette i t u (map parseColor cs) ws url
  parseJSON _  = mzero

parseColor :: String -> (Int,Int,Int)
parseColor [a,b,c,d,e,f] = (r [a,b], r [c,d], r [e,f])
  where r s = case readHex s of
                [(n, "")] -> n
                other     -> error $ "Can't parse color: "++show other
parseColor other = error $ "Error parsing color: " ++ other

paletteToGlossHaskellSrc :: Palette -> String
paletteToGlossHaskellSrc = unlines . map colorToGlossHaskellString . pColors

colorToGlossHaskellString (r,g,b) = 
  "makeColor8 " ++ unwords (map show [r,g,b, 255])

