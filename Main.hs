module Main where
import Graphics.Gloss.Interface.Game
import Json hiding (main)

main = guimain
guimain = do
  (PaletteList (pal:pals)) <- getPaletteList
  gameInWindow 
          "Color Lovers API demo" --name of the window
          (700,600) -- initial size of the window
          (0, 0) -- initial position of the window
          white -- background colour
          30 -- number of simulation steps to take for each second of real time
          (GS [] Red initTrain pal pals )-- the initial world
          drawState -- A function to convert the world into a picture
          handleInput -- A function to handle input events
          update

update :: Float -> GS -> GS
update step gs@(GS _msgs l ((tx,ty), tdir) pal pals) = gs { train = ((tx+vel, ty), tdir') }
  where vel = tdir * step * 40 * case l of
                Red -> 0
                Amber -> 1
                Green -> 2
        tdir' | tx > mx = -1
              | tx < (-mx) = 1
              | otherwise = tdir
          where mx = 550

initTrain = ((-500,0),1)
data GS = GS { msgs :: [Float] 
  , lightSt :: Light
  , train :: Train
  , palette:: Palette
  , palettes:: [Palette] } deriving (Show)

type ClickHandler = Event -> GS -> GS
handleInput :: Event -> GS -> GS
handleInput (EventKey (Char 'n') Down _ _)  gs = nextPalette gs
handleInput (EventKey (Char _c) Down _ _)  gs = gs
handleInput _ev@(EventKey (MouseButton LeftButton) Down  _ _pos) gs = 
  gs { lightSt = nextLight (lightSt gs) }

handleInput _ gs = gs 

nextPalette gs = 
  case palettes gs of
    [] -> gs
    (n:rest) -> gs { palette = n, palettes = rest }

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
drawState gs = Pictures [ 
--     drawLight (lightSt gs)
    drawPalette (-100, 40) $ palette gs
  , drawTrain (train gs) (palette gs) ]
  -- , drawDebug gs  ]

drawDebug :: GS -> Picture
drawDebug gs = color white $ 
  translate (-200) (-100) $ 
  scale 0.1 0.1 $ 
  Text $ show gs

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
