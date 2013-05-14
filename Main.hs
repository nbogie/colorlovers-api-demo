module Main where
import Control.Arrow ((&&&))
import Control.Concurrent(forkIO)
import Control.Concurrent.STM(readTChan,TChan,newTChan,isEmptyTChan,writeTChan,atomically)
import Data.Maybe (fromMaybe)
import Debug.Trace
import Graphics.Gloss.Interface.IO.Game
import System.Environment (getArgs)

import Palette hiding (main)
import qualified PaletteGetterWeb as PGW
import qualified PaletteGetterFile as PGF

main ::  IO ()
main = do
  args <- getArgs
  let display = if elem "-f" args then DMFull else DMWindow
  guimain display

data DisplayMode = DMWindow | DMFull

data ChanMsg = NewPaletteList PaletteList
             | Msg String
             deriving (Show)

guimain :: DisplayMode ->  IO ()
guimain dispMode = do
  ch   <- atomically $ newTChan :: IO (TChan ChanMsg) -- receives results of forked web requests, etc
  plE  <- PGF.getRandomPaletteList
  let (pal, pals) = case plE of
        Left err                   -> error $ "While getting or parsing palette: " ++ err
        Right (PaletteList (p:ps)) -> (p, ps)
        Right _                    -> error "empty palette list!"

  playIO
          (display dispMode)
          white -- background colour
          30 -- number of simulation steps to take for each second of real time
          (GS [] Slow (initTrain (winHeight dispMode)) pal pals [] False SDVert initColorControls) -- the initial world
          (return . drawState) -- A function to convert the world into a picture
          (handleInput ch) -- A function to handle input events
          (updateState ch)
  where
    winHeight DMFull = 1280
    winHeight DMWindow = 950
    display DMFull = FullScreen (winHeight DMFull, 800)
    display DMWindow = 
      (InWindow "Color Lovers Palettes demo" --name of the window
            (winHeight DMWindow,400) -- initial size of the window
            (0, 0) -- initial position of the window
      )




updateState :: (TChan ChanMsg) -> Float -> GS -> IO GS
updateState ch step gs = do
  processAnyMessages ch gs >>= return . updateTrain step 

updateTrain ::  Float -> GS -> GS
updateTrain step gs = 
  gs { train = ((tx+vel, ty), tdir') }
    where 
      vel             = tdir * step * 20 * stateMult
      stateMult       = case lightSt gs of
                          Stop -> 0
                          Slow -> 1 
                          Go -> 2
      ((tx,ty),tdir)  = train gs
      tdir' | tx > mx    = -1
            | tx < (-mx) = 1
            | otherwise  = tdir
        where mx = 450

processAnyMessages :: TChan ChanMsg -> GS -> IO GS
processAnyMessages ch gs = do
  chanEmpty <- atomically $ isEmptyTChan ch
  if not chanEmpty
      then
        fmap (processMessage gs) $ atomically (readTChan ch)
      else
        return gs
processMessage :: GS -> ChanMsg -> GS
processMessage gs (Msg _) = gs
processMessage gs (NewPaletteList (PaletteList (p:ps))) = 
  gs { palette = p, palettes = ps }
processMessage gs (NewPaletteList _) = gs -- empty palette - shouldn't have been queued

initTrain :: Float ->  ((Float, Float), Float)
initTrain windowHeight = ((-500,windowHeight * 0.1),1)
-- initTrain windowHeight = ((-500,0),1)

data GS = GS { msgs :: [Float] 
  , lightSt :: Light
  , train :: Train
  , palette:: Palette
  , palettes:: [Palette]
  , favourites :: [Palette]
  , displayInfo :: Bool
  , stripeDirection :: SDirection
  , colorControls :: ColorControls
  } deriving (Show)

data SDirection = SDVert | SDHoriz | SDSlant deriving (Show, Eq)
data PaletteSrc = FromWeb | FromFile

getPaletteList ::  PaletteSrc -> IO (Either String PaletteList)
getPaletteList FromWeb  = PGW.getRandomPaletteList
getPaletteList FromFile = PGF.getRandomPaletteList

randomPaletteList :: (TChan ChanMsg) ->  PaletteSrc -> IO ()
randomPaletteList ch src = do
        _tId <- forkIO$ do
            rp <- getPaletteList src
            case rp of
              Left _err                                 -> return ()
              Right pl@(PaletteList ps) | not $ null ps -> atomically $ writeTChan ch (NewPaletteList pl)
                                        | otherwise     -> error "Empty palette list in getPaletteList"
        return ()


handleInput :: (TChan ChanMsg) -> Event -> GS -> IO GS
handleInput ch (EventKey (SpecialKey KeySpace) Down _ _) gs = randomPaletteList ch FromWeb >> return gs
handleInput ch (EventKey (Char 'l')            Down _ _) gs = randomPaletteList ch FromFile >> return gs
handleInput _  (EventKey k                     Down _ _) gs = return $ handleDown k gs
handleInput _ _                                          gs = return gs -- ignore key ups, and other

handleDown ::  Key -> GS -> GS
handleDown (SpecialKey KeyEnter) = forwardLight
handleDown (Char       'f')      = addToFavourites
handleDown (Char       'F')      = seeFavourites
handleDown (Char       'i')      = toggleInfo
handleDown (Char       'r')      = toggleStripeDirection
handleDown (Char       'd')      = chgColorCtrls toggleDimType
handleDown (SpecialKey KeyDown)  = chgColorCtrls forwardDim
handleDown (SpecialKey KeyUp)    = chgColorCtrls backDim
handleDown (SpecialKey KeyLeft)  = forwardPalette
handleDown (SpecialKey KeyRight) = backPalette
handleDown (MouseButton LeftButton) = forwardLight
handleDown _ = id

summariseFavourites :: GS -> [String]
summariseFavourites gs = map (show . (pId &&& pTitle)) fs
  where fs = (favourites gs)

seeFavourites :: GS -> GS
seeFavourites gs = trace (unlines $ summariseFavourites gs) $ 
  case favourites gs of
  [] -> gs
  (p:ps) -> gs { palette = p, palettes = ps }

addToFavourites :: GS -> GS
addToFavourites gs = gs { favourites = palette gs : favourites gs } 

toggleDimType :: ColorControls -> ColorControls
toggleDimType (ColorControls da dt) = ColorControls da (otherDimType dt)
 
forwardDim, backDim ::  ColorControls -> ColorControls
forwardDim (ColorControls dimAmount dimType) = ColorControls (changeDim 1 dimAmount) dimType
backDim    (ColorControls dimAmount dimType) = ColorControls (changeDim (-1) dimAmount) dimType
changeDim ::  (Num a, Ord a) => a -> a -> a
changeDim inc v = minimum [maximum [v + inc, minCap], maxCap]
  where (minCap, maxCap) = (-3,3)

toggleInfo ::  GS -> GS
toggleInfo gs = gs {displayInfo = not $ displayInfo gs}

toggleStripeDirection ::  GS -> GS
toggleStripeDirection gs = gs {stripeDirection = t $ stripeDirection gs } 
  where t SDVert = SDHoriz
        t SDHoriz = SDSlant
        t SDSlant = SDVert

forwardPalette ::  GS -> GS
forwardPalette gs = let current = palette gs in
  case palettes gs of
    [] -> gs
    (n:rest) -> gs { palette = n, palettes = rest ++ [current] }

backPalette ::  GS -> GS
backPalette gs = 
  case palettes gs of
    [] -> gs
    (ps) -> gs { palette = fromEnd, palettes = current : rest }
      where (fromEnd, rest) = (last ps, init ps)
            current = palette gs

forwardLight ::  GS -> GS
forwardLight gs = gs { lightSt = nextLight (lightSt gs) }

data Light = Stop | Go | Slow deriving (Show, Eq)
nextLight :: Light -> Light
nextLight Stop = Go
nextLight Go = Slow
nextLight Slow = Stop

drawState :: GS -> Picture
drawState gs = Pictures $ [ 
    drawBackground $ palette gs
--   , translate (0) 0 $ drawPaletteCube $ palette gs
  , drawPalette2 (-100) (stripeDirection gs) $ palette gs
  , drawTrain (train gs) (palette gs) (colorControls gs) ]
    ++ [drawInfo gs | displayInfo gs]

drawBackground :: Palette -> Picture
drawBackground _p = rectangleSolid 2000 2000 
drawInfo :: GS -> Picture
drawInfo gs = color white $ 
  translate (-300) (-100) $ 
  scale 0.1 0.1 $ 
  Text $ pTitle $ palette gs

type Train = ((Float,Float), Float)

drawPalette ::  (Float, Float) -> Palette -> Picture
drawPalette (tx,ty) pal = Pictures $ zipWith draw [0,100..] (pGlossColors pal)
  where draw x = stripe (tx + x, ty)

drawPaletteCube :: Palette -> Picture
drawPaletteCube pal = scale 50 50 $ Pictures $ reverse $ zipWith cube colors [1..]
  where
    cube col scl = Color col $ scale scl scl $ rectangleSolid 1 1
    colors = pGlossColors pal

drawPalette2 ::  Float -> SDirection -> Palette -> Picture
drawPalette2 tx sdir pal = rotate rotAmt $ Pictures $ zipWith draw (posAndWidths widths) (pGlossColors pal)
  where 
    draw (x,w) = stripeWidth (tx + x) w
    widths = map (*500) $ fromMaybe defaultWidths (pWidths pal) 
      where defaultWidths = take (numColors pal) $ repeat 0.2
    rotAmt | sdir == SDHoriz = 90
           | sdir == SDSlant = 35
           | otherwise       = 0

posAndWidths widths = zip midPositions widths
  where
    midPositions = reverse $ fst $ foldl f ([], 0) widths
      where
        f (midPosns, edge) w = ( edge + w/2 : midPosns, edge + w)

pGlossColors :: Palette -> [Color]
pGlossColors = map toGlossColor . pColors
toGlossColor ::  (Int, Int, Int) -> Color
toGlossColor (r,g,b) = makeColor8 r g b 255

stripeWidth :: Float -> Float -> Color -> Picture
stripeWidth x w c = 
  translate x 0 $ Pictures [
    color c $ rectangleSolid w 3000]

stripe :: (Float, Float) -> Color -> Picture
stripe (x,y) c = 
  translate x y $ Pictures [
    color c $ rectangleSolid 30 3000]

drawTrain :: Train -> Palette -> ColorControls -> Picture
drawTrain ((tx,ty),_td) pal colCtrls = Pictures $ zipWith draw [0,100..] (pGlossColors pal)
 where draw x = drawCar (tx + x, ty) colCtrls

drawCar :: (Float, Float) -> ColorControls -> Color -> Picture
drawCar (x,y) (ColorControls dimAmount dimType) c = rotate 0 $ Pictures [
  translate x y $ Pictures [
    translate (-20) 0 $ color wheelColor $ circleSolid 10,
    translate 20    0 $ color wheelColor $ circleSolid 10,
    translate 0    10 $ Pictures [
      color c $ rectangleSolid 80 30,
      translate 0 15 $ color shadeColor $ rectangleSolid 80 15]
    ]]
  where shadeColor = last $ take (abs dimAmount + 1) $ iterate f c
        wheelColor = f c 
        f = if dimAmount < 0 then brightOrLight dimType else dimOrDark dimType

----------------------------------------------------------------------------------
-- ColorControls
----------------------------------------------------------------------------------
type DimAmount = Int
data DimType = LightAndDark | BrightAndDim  deriving (Show)
data ColorControls = ColorControls DimAmount DimType deriving (Show)

chgColorCtrls :: (ColorControls -> ColorControls) -> GS -> GS
chgColorCtrls f gs@(GS{ colorControls=cc }) = gs {colorControls = f cc}

initColorControls ::  ColorControls
initColorControls = forwardDim $ ColorControls 0 BrightAndDim

otherDimType ::  DimType -> DimType
otherDimType BrightAndDim = LightAndDark
otherDimType LightAndDark = BrightAndDim

brightOrLight ::  DimType -> Color -> Color
brightOrLight LightAndDark = light
brightOrLight BrightAndDim = bright

dimOrDark ::  DimType -> Color -> Color
dimOrDark     LightAndDark = dark
dimOrDark     BrightAndDim = dim

