{-# LANGUAGE OverloadedStrings #-} 
module Json where
--
-- example urls for palettes
-- http://www.colourlovers.com/api/palette/1244?format=json
-- http://www.colourlovers.com/api/palettes/search?sortCol=votes&sortBy=DESC&query=candy&format=json

-- WARNING: 
-- This is a TERRIBLE example of Aeson / Attoparsec.
-- Don't do things this way.
--
-- import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson
import qualified Data.Aeson.Types as T
import Data.Attoparsec (parse, Result(..))
import qualified Data.ByteString.Char8 as BS

import Numeric (readHex)

testFile = "inputs/many.json" -- "inputs/example.json"

main ::IO ()
main = getPaletteList >>= print

getPaletteList ::  IO PaletteList
getPaletteList = do
  result <- (fmap parseFromString $ readFile testFile)
  case result of
    Success kc -> return kc
    Error e -> error $ "Error parsing read json: " ++ e

data PaletteList = PaletteList [Palette] deriving (Show)
data Palette = Palette 
  { pId :: Integer
  , pTitle::String
  , pUserName::String
  , pColors :: [(Int, Int, Int)]
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
    url  <-  o  .:  "url"
    return $ Palette i t u (map parseColor cs) url
  parseJSON _  = mzero

parseColor :: String -> (Int,Int,Int)
parseColor [a,b,c,d,e,f] = (r [a,b], r [c,d], r [e,f])
  where r s = case readHex s of
                [(n, "")] -> n
                other     -> error $ "Can't parse color: "++show other
parseColor other = error $ "Error parsing color: " ++ other

parseFromString :: String -> T.Result PaletteList
parseFromString s = 
  let bs = BS.pack s
  in case parse json bs of
       Done _rest result -> T.parse parseJSON result
       Fail rest ctxts err -> 
         Error $ "JSON parse error: " ++ err ++ ", contexts: " 
                 ++ show ctxts  ++ ", rest: " ++ BS.unpack rest
       Partial _           -> 
         Error "JSON parse error.  Unexpected partial."
