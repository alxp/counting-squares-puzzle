import Graphics.Gloss
import Model (ShapeLine, Drawing, allSquaresOnDrawing)

myDrawing :: Drawing
myDrawing = [((0, 0), (400, 0)), ((400, 0), (400, 400)),
             ((0, 0), (0, 400)), ((0, 400), (400, 400)),
             ((100, 0), (100, 400)), ((200, 0), (200, 400)),
             ((300, 0), (300, 400)),
             ((0, 100), (400, 100)), ((0, 200), (400, 200)),
             ((0, 300), (400, 300)),
             ((150, 50), (250, 50)), ((150, 50), (150, 150)),
             ((150, 150), (250, 150)), ((250, 50), (250, 150)),
             ((150, 250), (250, 250)), ((150, 250), (150, 350)),
             ((150, 350), (250, 350)), ((250, 250), (250, 350))]
             

drawDrawing :: Drawing -> Picture
drawDrawing d = pictures $ map drawLine d

drawLine :: ShapeLine -> Picture
drawLine ((ah, av), (bh, bv)) = Line [(fromIntegral ah, fromIntegral av), (fromIntegral bh, fromIntegral bv)]

-- Draw a square given its bottom line
drawSolidSquare :: ShapeLine -> Picture
drawSolidSquare ((ah, av), (bh, _)) = translate (offset ah) (offset av) $ rectangleSolid h h
  where h = fromIntegral (bh - ah)
        offset n = ((fromIntegral n) + h / 2)

-- Produce one frame of the animation
frame :: Float -> Picture
frame timeS = translate (-250) (-225) $ pictures $
              [ drawDrawing myDrawing
              , drawSolidSquare (allSquaresOnDrawing myDrawing !! step timeS)
              , translate 410.0 10.0 $ text $ show $ (step timeS) + 1
              ]
  -- Stop stepping the animation when we run out of squares
  where step t = min (floor t) ((length (allSquaresOnDrawing myDrawing)) - 1)

main :: IO ()
main = animate (InWindow "Squares" (700, 500) (20, 20)) white $ frame