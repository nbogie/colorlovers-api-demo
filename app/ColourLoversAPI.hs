module PaletteGetterWeb where
import Control.Applicative ((<$>),(<*>))
import Control.Monad (guard)
import Data.Char (toUpper)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import HttpUtils (getBody)
import Numeric (readHex, showHex)

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
type URLString = String
type ParamName = String
type ParamValue= String

class CLParam a where
  encodeAsParams :: a -> URLString

class HasParamValue a where
  paramValue :: a -> ParamValue


data HexLogic = AnyHexFrom | AllHexesFrom deriving (Show)

instance CLParam HexLogic where
  encodeAsParams hl = "hex_logic" `pair` hl

instance HasParamValue HexLogic where
  paramValue AnyHexFrom   = "AND"
  paramValue AllHexesFrom = "OR"

instance CLParam HexList where
  encodeAsParams (HexList hs) = 
    "hex" .= joinPVs "," hs

(.=) :: ParamName -> ParamValue -> URLString
k .= v = k ++ "=" ++ v
(.:):: (HasParamValue a) => ParamName -> a -> URLString
(.:) = pair

pair :: (HasParamValue a) => ParamName -> a -> URLString
pair k v = k ++ "=" ++ (paramValue v)

data HexChoice = HexChoice HexLogic HexList deriving (Show)
data HexList = HexList [Hex] deriving (Show)
data Hex = Hex Int Int Int deriving (Show)

instance CLParam HexChoice where
  encodeAsParams (HexChoice logic hs) = intercalate "&" 
    [ encodeAsParams logic
    , encodeAsParams hs
    ] 

allHexesFrom :: [String] -> Either String HexChoice
allHexesFrom strs = AllHexesFrom `makeHexes` strs

anyHexFrom :: [String] -> Either String HexChoice
anyHexFrom  strs = AnyHexFrom `makeHexes` strs

makeHexes ::  HexLogic -> [String] -> Either String HexChoice
makeHexes hexLogic strs = 
  if (length strs <= 5)  -- use guard, but for overlapping instances?
    then fmap (HexChoice hexLogic . HexList) $ sequence $ map toHex strs 
    else Left "More than 5 values in hex choice"

--TODO: duplicated in Json module
toHex ::  [Char] -> Either String Hex
toHex str@[a,b,c,d,e,f] = Hex <$> r [a,b] <*> r [c,d] <*> r [e,f]
  where 
    r pair = case readHex pair of
                [(n, "")] -> return n
                _other    -> Left $ "Invalid hex code: " ++ pair ++ " in " ++ str
toHex other = Left $ "Color hex string wrong size: " ++ other

instance HasParamValue Hex where
  paramValue (Hex r g b ) = concatMap f [r,g,b]
    where 
        f :: Int -> String
        f i = map toUpper $ padTwo $ showHex i ""
        padTwo [x] = ['0',x]
        padTwo x   = x

instance CLParam HueOption where
  encodeAsParams (HueOption hs) = 
    "hueOption" .= joinPVs "," hs

joinPVs :: HasParamValue a => String -> [a] -> String
joinPVs sep = intercalate sep . map paramValue

data HueOption = HueOption [CLHue] deriving (Show)

data CLHue = CLYellow | CLOrange | CLRed | CLGreen 
           | CLViolet | CLBlue | CLAqua 
           | CLFuchsia 
           deriving (Show)

instance HasParamValue CLHue where
  paramValue CLYellow  = "yellow"
  paramValue CLOrange  = "orange"
  paramValue CLRed     = "red"
  paramValue CLGreen   = "green"
  paramValue CLViolet  = "violet"
  paramValue CLBlue    = "blue"
  paramValue CLAqua    = "aqua"
  paramValue CLFuchsia = "fuchsia"

instance CLParam SortBy where
  encodeAsParams s = "sortBy" .: s
instance HasParamValue SortBy where
  paramValue SortAscending = "ASC"
  paramValue SortDescending = "DESC"

data SortBy = SortAscending | SortDescending deriving (Show)

data NumResults = NumResults Int deriving (Show)
numResults :: Int -> Either String NumResults
numResults i | i < 0 || i > 100 = Left "Illegal num results (must be between 1 and 100)"
             | otherwise        = Right $ NumResults i

data OrderCol = OrderDateCreated | OrderScore | OrderName | OrderNumVotes | OrderNumViews deriving (Show)
instance CLParam OrderCol where
  encodeAsParams oc = "orderCol" .: oc

instance HasParamValue OrderCol where
  paramValue OrderDateCreated = "dateCreated"
  paramValue OrderScore       = "score"
  paramValue OrderName        = "name"
  paramValue OrderNumVotes    = "numVotes"
  paramValue OrderNumViews    = "numViews"


data Keyword = Keyword String deriving (Show)
data KeywordsChoice = KeywordsChoice KeywordExact KeywordList deriving (Show)

instance CLParam KeywordsChoice where
  encodeAsParams (KeywordsChoice exactness klist) = 
    "keywordExact" .: exactness
    ++ encodeAsParams klist

instance HasParamValue KeywordExact where
  paramValue KeywordExact = "1"
  paramValue KeywordInexact = "0"

data KeywordList = KeywordList [Keyword] deriving (Show)

instance HasParamValue Keyword where
  paramValue (Keyword k) = k
  -- (intercalate " " [k | Keyword k <- ks]) -- TODO url encode into space separated list.  what to do about existing spaces in keywords?


instance CLParam KeywordList where
  encodeAsParams (KeywordList ks) = 
    "keywords" .= joinPVs " " ks

data KeywordExact = KeywordExact | KeywordInexact deriving (Show)
data Request = Request 
                   (Maybe HueOption)
                   (Maybe HexChoice)
                   (Maybe KeywordsChoice)
                   (Maybe OrderCol)
                   (Maybe SortBy)
                   deriving (Show)

encodeRequest :: Request -> URLString
encodeRequest (Request hue hex ks o sort) = 
  intercalate "&" $ catMaybes 
    [ fmap encodeAsParams hue
    , fmap encodeAsParams hex
    , fmap encodeAsParams ks
    , fmap encodeAsParams o
    , fmap encodeAsParams sort
    ]

demoRequest = 
  Request Nothing
          (Just $ unsafeFromRight $ allHexesFrom ["02FF33", "CC00FF", "DD0033"])
          Nothing
          (Just OrderDateCreated)
          (Just SortDescending)
   where 
     unsafeFromRight (Left x) = error "unexpected left value in demoRequest"
     unsafeFromRight (Right x) = x
main = putStrLn $ encodeRequest demoRequest
