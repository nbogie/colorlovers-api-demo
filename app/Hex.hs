module Hex
import Encoder

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


toHexString :: Hex -> String
toHexString (Hex r g b ) = concatMap f [r,g,b]
  where 
        f :: Int -> String
        f i = padTwo $ showHex i ""
        padTwo [x] = ['0',x]
        padTwo x   = x
