module Main where
import Graphics.Gloss.Interface.Game
import Json hiding (main)

main = guimain
guimain = do
  (PaletteList (pal:pals)) <- getPaletteList
  gameInWindow 
          "Color Lovers Palettes demo" --name of the window
          (950,400) -- initial size of the window
          (0, 0) -- initial position of the window
          white -- background colour
          30 -- number of simulation steps to take for each second of real time
          (GS [] Red initTrain pal pals False) -- the initial world
          drawState -- A function to convert the world into a picture
          handleInput -- A function to handle input events
          updateState

updateState :: Float -> GS -> GS
updateState step gs = gs { train = ((tx+vel, ty), tdir') }
  where vel = tdir * step * 20 * case lightSt gs of
                Red -> 0
                Amber -> 1
                Green -> 2
        ((tx,ty),tdir) = train gs
        tdir' | tx > mx = -1
              | tx < (-mx) = 1
              | otherwise = tdir
          where mx = 550

initTrain = ((-500,0),1)
data GS = GS { msgs :: [Float] 
  , lightSt :: Light
  , train :: Train
  , palette:: Palette
  , palettes:: [Palette]
  , displayInfo :: Bool } deriving (Show)

handleInput :: Event -> GS -> GS
handleInput (EventKey k Down _ _) = handleDown k
handleInput _ = id 

handleDown (Char       ' ')      = forwardLight
handleDown (Char       'i')      = toggleInfo
handleDown (SpecialKey KeyLeft)  = forwardPalette
handleDown (SpecialKey KeyRight) = backPalette
handleDown (MouseButton LeftButton) = forwardLight
handleDown _ = id

toggleInfo gs = gs {displayInfo = not $ displayInfo gs}
forwardPalette gs = let current = palette gs in
  case palettes gs of
    [] -> gs
    (n:rest) -> gs { palette = n, palettes = rest ++ [current] }

backPalette gs = 
  case palettes gs of
    [] -> gs
    (ps) -> gs { palette = fromEnd, palettes = current : rest }
      where (fromEnd, rest) = (last ps, init ps)
            current = palette gs

forwardLight gs = gs { lightSt = nextLight (lightSt gs) }

data Light = Red | Green | Amber deriving (Show, Eq)
nextLight :: Light -> Light
nextLight Red = Green
nextLight Green = Amber
nextLight Amber = Red
class ColorFor a where
  colorFor :: a -> Color

instance ColorFor Light where
  colorFor Red = red
  colorFor Green = green
  colorFor Amber = orange

drawState :: GS -> Picture
drawState gs = Pictures $ [ 
--     drawLight (lightSt gs)
    drawPalette (-100, 40) $ palette gs
  , drawTrain (train gs) (palette gs) ] 
    ++ [drawInfo gs | displayInfo gs]

drawInfo :: GS -> Picture
drawInfo gs = color black $ 
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

drawTrain :: Train -> Palette -> Picture
drawTrain ((tx,ty),_td) pal = Pictures $ zipWith draw [0,100..] (pGlossColors pal)
 where draw x = drawCar (tx + x, ty)

drawCar :: (Float, Float) -> Color -> Picture
drawCar (x,y) c = Pictures [
  translate x y $ Pictures [
    translate (-20) 0 $ color black $ circleSolid 10,
    translate 20    0 $ color black $ circleSolid 10,
    translate 0 10    $ color c $ rectangleSolid 80 30
    ]]

drawLight tlight = translate (-200) 100 $ scale 0.1 0.1 $ 
  Pictures [ 
  Color black $ squareSolid 290,
  Color (colorFor tlight) $ circleSolid 100  ]
squareSolid s = rectangleSolid s s 
