module Model where

import Data.List

type LinePoint = (Integer, Integer)
type ShapeLine = (LinePoint, LinePoint)
type Drawing = [ShapeLine]


isASquare :: Drawing -> ShapeLine -> Bool
isASquare d l = and [drawingContainsSegment d s | s <- squareOfBottomLine l]

-- Generate three more lines to form a square out of the bottom line.
squareOfBottomLine :: ShapeLine -> [ShapeLine]
squareOfBottomLine ((ah, av), (bh, bv)) 
  | av /= bv = []
  | otherwise = [((ah, av), (bh, bv)), ((ah, av + l), (bh, bv + l)),
                 ((ah, av), (ah, av + l)), ((bh, bv), (bh, bv + l))]
  where l = bh - ah

drawingContainsSegment :: Drawing -> ShapeLine -> Bool
drawingContainsSegment [] _ = False
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

-- Implement the GT function to allow line segments to be sorted
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

-- Get the horizontal points where a vertical line crosses the given horizontal line
-- This excludes the rightmost point which is fine since it can't form a square.
pointsCrossing :: Drawing -> ShapeLine -> [Integer]
pointsCrossing d l = [fst (fst s) | s <- sortHorizontally (verticalLinesCrossing d l)]

allSquaresOnLine :: Drawing -> ShapeLine -> [ShapeLine]
allSquaresOnLine d l = [s | s <- horizontalSegments d l, isASquare d s]

allSquaresOnDrawing :: Drawing -> [ShapeLine]
allSquaresOnDrawing d = concat $ map (allSquaresOnLine d) (horizontalLines d)

lineContainsSegment :: ShapeLine -> ShapeLine -> Bool
lineContainsSegment ((ah, av), (bh, bv)) ((ch, cv), (dh, dv))
  | av == bv = av == cv && bv == dv && ah <= ch && bh >= dh 
  | ah == bh = ah == ch && bh == dh && av <= cv && bv >= dv
  | otherwise = False

