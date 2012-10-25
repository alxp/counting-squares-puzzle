import Graphics.Gloss
import Data.List
import Model

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