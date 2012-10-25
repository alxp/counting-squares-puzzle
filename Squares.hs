import Graphics.Gloss
import Model (ShapeLine, Drawing, allSquaresOnDrawing)

myDrawing :: Drawing
myDrawing = [((0, 0), (400, 0)), ((400, 0), (400, 400)),
             ((0, 0), (0, 400)), ((0, 400), (400, 400)),
             ((100, 0), (100, 400)), ((200, 0), (200, 400)),
             ((300, 0), (300, 400)),
             ((0, 100), (400, 100)), ((0, 200), (400, 200)),
             ((0, 300), (400, 300)),
             ((150, 150), (250, 150)), ((150, 250), (250, 250)),
             ((150, 150), (150, 250)), ((250, 150), (250, 250))]

drawDrawing :: Drawing -> Picture
drawDrawing d = pictures $ map drawLine d

drawLine :: ShapeLine -> Picture
drawLine ((lp1, lp2), (lp3, lp4)) = Line [(fromIntegral lp1, fromIntegral lp2), (fromIntegral lp3, fromIntegral lp4)]

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