module Gui where
import Graphics.Gloss.Interface.Game
import qualified Data.Map as M
import Json hiding (main)

main = guimain
guimain = do
  (PaletteList [palette]) <- getPalette
  gameInWindow 
          "HDTP Ex41" --name of the window
          (700,600) -- initial size of the window
          (0, 0) -- initial position of the window
          white -- background colour
          30 -- number of simulation steps to take for each second of real time
          ([], Red, initTrain, palette) -- the initial world
          drawState -- A function to convert the world into a picture
          handleInput -- A function to handle input events
          update

update :: Float -> GS -> GS
update step (_msgs, l, ((tx,ty), tdir), pal) = ([step], l, ((tx+vel, ty), tdir'), pal)
  where vel = tdir * step * 20 * case l of
                Red -> 0
                Amber -> 1
                Green -> 2
        tdir' | tx > 800 = -1
              | tx < 800 = 1
              | otherwise = tdir

initTrain = ((-300,0),1)
type GS = ([Float], Light,Train, Palette)

type ClickHandler = Event -> GS -> GS
handleInput :: Event -> GS -> GS
handleInput (EventKey (Char _c) Down _ _)  gs = gs
handleInput _ev@(EventKey (MouseButton LeftButton) Down  _ _pos) (is,l,c, pal) = (is,nextLight l,c, pal)
handleInput _ gs = gs 

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
drawState _gs@(msgs,_light,train,pal) = Pictures [ 
--     drawLight light
    drawPalette (-100, 40) pal
  , drawTrain train pal ]
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

drawLight light = translate (-200) 100 $ scale 0.1 0.1 $ 
  Pictures [ 
  Color black $ squareSolid 290,
  Color (colorFor light) $ circleSolid 100  ]
squareSolid s = rectangleSolid s s 
