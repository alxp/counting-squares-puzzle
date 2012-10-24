import Graphics.Gloss
import Data.List

type LinePoint = (Integer, Integer)
type ShapeLine = (LinePoint, LinePoint)

data Direction = Horizontal | Vertical 

myShapeLine :: ShapeLine
myShapeLine = ((0, 0), (150, 0))

type Drawing = [ShapeLine]

myDrawing :: Drawing
myDrawing = [((0, 0), (400, 0)), ((400, 0), (400, 400)),
             ((0, 0), (0, 400)), ((0, 400), (400, 400)),
             ((100, 0), (100, 400)), ((200, 0), (200, 400)),
             ((300, 0), (300, 400)),
             ((0, 100), (400, 100)), ((0, 200), (400, 200)),
             ((0, 300), (400, 300)),
             ((150, 150), (250, 150)), ((150, 250), (250, 250)),
             ((150, 150), (150, 250)), ((250, 150), (250, 250))]

isASquare :: Drawing -> ShapeLine -> Bool
isASquare d l = and [drawingContainsSegment d s | s <- squareOfBottomLine l]

squareOfBottomLine :: ShapeLine -> [ShapeLine]
squareOfBottomLine ((ah, av), (bh, bv)) 
  | av /= bv = []
  | otherwise = [((ah, av), (bh, bv)), ((ah, av + l), (bh, bv + l)),
                 ((ah, av), (ah, av + l)), ((bh, bv), (bh, bv + l))]
  where l = bh - ah

drawingContainsSegment :: Drawing -> ShapeLine -> Bool
drawingContainsSegment [] s = False
drawingContainsSegment (l: ls) s = lineContainsSegment l s || drawingContainsSegment ls s

drawingContainsSquare :: Drawing -> ShapeLine -> Bool
drawingContainsSquare d l = and $ map (drawingContainsSegment d) (squareOfBottomLine l)

horizontalLines :: Drawing -> [ShapeLine]
horizontalLines d = [l | l <- d, isHorizontal l]


isHorizontal :: ShapeLine -> Bool
isHorizontal ((_, av), (_, bv)) = av == bv

verticalLines :: Drawing -> [ShapeLine]
verticalLines d = [l | l <- d, isVertical l]

isVertical :: ShapeLine -> Bool
isVertical ((ah, _), (bh, _)) = ah == bh

sortHorizontallyGT ((ah, _), (_, _)) ((ch, _), (_, _)) 
  | ah < ch = LT
  | ah > ch = GT
  | ah == ch = EQ

sortHorizontally :: [ShapeLine] -> [ShapeLine]
sortHorizontally = sortBy sortHorizontallyGT 

verticalLinesCrossing :: Drawing -> ShapeLine -> [ShapeLine]
verticalLinesCrossing d ((ah, av), (bh, bv))
  | not $ isHorizontal ((ah, av), (bh, bv)) = []
  | otherwise = [((ch, cv), (dh, dv)) | ((ch, cv), (dh, dv)) <- verticalLines d,
                 cv <= av && dv >= av && ch >= ah && dh <= bh]

-- Get all combinations of horizontal line segments on the given line
horizontalSegments :: Drawing -> ShapeLine -> [ShapeLine]
horizontalSegments d ((ah, av), (bh, _)) = [((ch, cv), (dh, dv)) | ch <- ps, dh <- ps, cv <- [av], dv <- [av], dh > ch]
  where ps = pointsCrossing d ((ah, av), (bh, av))

allSquaresOnLine :: Drawing -> ShapeLine -> [ShapeLine]
allSquaresOnLine d l = [s | s <- horizontalSegments d l, isASquare d s]

pointsCrossing :: Drawing -> ShapeLine -> [Integer]
pointsCrossing d l = [fst (fst s) | s <- sortHorizontally (verticalLinesCrossing d l)]

lineContainsSegment :: ShapeLine -> ShapeLine -> Bool
lineContainsSegment ((a1h, a1v), (a2h, a2v)) ((b1h, b1v), (b2h, b2v))
  | a1v == a2v = a1v == b1v && a2v == b2v && a1h <= b1h && a2h >= b2h 
  | a1h == a2h = a1h == b1h && a2h == b2h && a1v <= b1v && a2v >= b2v
  | otherwise = False

pointsInSegment :: ShapeLine -> [LinePoint]
pointsInSegment ((ah, av), (bh, bv)) = [(ah, av), (bh, bv)]

drawDrawing :: Drawing -> Picture
drawDrawing d = pictures $ map drawLine d

drawLine :: ShapeLine -> Picture
drawLine ((lp1, lp2), (lp3, lp4))  = Line [(fromIntegral lp1, fromIntegral lp2), (fromIntegral lp3, fromIntegral lp4)]

drawMyLines :: Picture
drawMyLines = translate (-250) (-250) $ drawDrawing myDrawing

main = display (InWindow "Squares" (550, 550) (20, 20)) white $ drawMyLines