module Main where
import Control.Concurrent
import Debug.Trace
import Graphics.Gloss.Interface.IO.Game

import Json hiding (main)
import qualified PaletteGetterWeb as PGW
import qualified PaletteGetterFile as PGF

main ::  IO ()
main = guimain

data ChanMsg = NewPaletteList PaletteList
             | Msg String
             deriving (Show)

guimain ::  IO ()
guimain = do
  ch   <- newChan :: IO (Chan ChanMsg) -- receives results of forked web requests, etc
  plE  <- PGF.getRandomPaletteList
  let (pal, pals) = case plE of
        Left err                   -> error $ "While getting or parsing palette: " ++ err
        Right (PaletteList (p:ps)) -> (p, ps)
        Right _                    -> error "empty palette list!"

  playIO
          (InWindow "Color Lovers Palettes demo" --name of the window
                (950,400) -- initial size of the window
                (0, 0) -- initial position of the window
          )
          white -- background colour
          30 -- number of simulation steps to take for each second of real time
          (GS [] Slow initTrain pal pals False initColorControls) -- the initial world
          (return . drawState) -- A function to convert the world into a picture
          (handleInput ch) -- A function to handle input events
          (updateState ch)



updateState :: (Chan ChanMsg) -> Float -> GS -> IO GS
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

processAnyMessages :: Chan ChanMsg -> GS -> IO GS
processAnyMessages ch gs = do
  chanEmpty <- isEmptyChan ch
  if not chanEmpty
      then
        fmap (processMessage gs) $ readChan ch
      else
        return gs
processMessage :: GS -> ChanMsg -> GS
processMessage gs (Msg _) = gs
processMessage gs (NewPaletteList (PaletteList (p:ps))) = 
  gs { palette = p, palettes = ps }
processMessage gs (NewPaletteList _) = gs -- empty palette - shouldn't have been queued

initTrain ::  ((Float, Float), Float)
initTrain = ((-500,0),1)

data GS = GS { msgs :: [Float] 
  , lightSt :: Light
  , train :: Train
  , palette:: Palette
  , palettes:: [Palette]
  , displayInfo :: Bool
  , colorControls :: ColorControls
  } deriving (Show)

data PaletteSrc = FromWeb | FromFile

getPaletteList ::  PaletteSrc -> IO (Either String PaletteList)
getPaletteList FromWeb  = PGW.getRandomPaletteList
getPaletteList FromFile = PGF.getRandomPaletteList

randomPaletteList :: (Chan ChanMsg) ->  PaletteSrc -> IO ()
randomPaletteList ch src = do
        _tId <- forkIO$ do
            rp <- getPaletteList src
            case rp of
              Left _err                     -> traceShow _err (return ())
              Right pl@(PaletteList ps) | not $ null ps -> writeChan ch (NewPaletteList pl)
                                        | otherwise      -> error "Empty palette list in getPaletteList"
        return ()


handleInput :: (Chan ChanMsg) -> Event -> GS -> IO GS
handleInput ch (EventKey (SpecialKey KeySpace) Down _ _) gs = randomPaletteList ch FromWeb >> return gs
handleInput ch (EventKey (Char 'f')            Down _ _) gs = randomPaletteList ch FromFile >> return gs
handleInput _  (EventKey k                     Down _ _) gs = return $ handleDown k gs
handleInput _ _                                         gs = return gs -- ignore key ups, and other

handleDown ::  Key -> GS -> GS
handleDown (SpecialKey KeyEnter) = forwardLight
handleDown (Char       'i')      = toggleInfo
handleDown (Char       'd')      = chgColorCtrls toggleDimType
handleDown (SpecialKey KeyDown)  = chgColorCtrls forwardDim
handleDown (SpecialKey KeyUp)    = chgColorCtrls backDim
handleDown (SpecialKey KeyLeft)  = forwardPalette
handleDown (SpecialKey KeyRight) = backPalette
handleDown (MouseButton LeftButton) = forwardLight
handleDown _ = id

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
  , drawPalette (-100, 40) $ palette gs
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

drawPalette (tx,ty) pal = Pictures $ zipWith draw [0,100..] (pGlossColors pal)
  where draw x = stripe (tx + x, ty)

pGlossColors :: Palette -> [Color]
pGlossColors = map toGlossColor . pColors
toGlossColor (r,g,b) = makeColor8 r g b 255

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

initColorControls = ColorControls 0 BrightAndDim
otherDimType BrightAndDim = LightAndDark
otherDimType LightAndDark = BrightAndDim
brightOrLight LightAndDark = light
brightOrLight BrightAndDim = bright
dimOrDark     LightAndDark = dark
dimOrDark     BrightAndDim = dim

