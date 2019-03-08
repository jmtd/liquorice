{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Liquorice.Line( Line(..)
                     , Point
                     , flipline
                     , splitLine
                     , checkIntersect
                     , splitLines
                     , workbest
                     , htf_thisModulesTests
                     ) where

import Data.List
import Test.Framework
import Control.Monad

type Point = (Int, Int)

data Line = Line { from    :: Point
                 , to      :: Point
                 , lineTop :: String
                 , lineMid :: String
                 , lineBot :: String
                 , lineType:: Int
                 , lineTag :: Int
                 , lineXoff:: Int
                 , lineYoff:: Int
                 } deriving (Show, Eq)

flipline :: Line -> Line
flipline l = l { from = to l, to = from l }

lineOrient :: Line -> Ordering
lineOrient l = let (a,b) = (from l, to l) in if a < b then GT else LT

checkIntersect :: Line -> Line -> Bool
checkIntersect a b = (sameXAxis a b || sameYAxis a b) && newCheckIntersect a b

sameXAxis x y = all (==(fst (from x))) (map fst [to x, from y, to y])
sameYAxis x y = all (==(snd (from x))) (map snd [to x, from y, to y])

-- take advantage of tuple Ord
newCheckIntersect :: Line -> Line -> Bool
newCheckIntersect l1 l2 = let nl1 = normalizeLine l1
                              nl2 = normalizeLine l2
                              (x1,y1) = (from nl1, to nl1)
                              (x2,y2) = (from nl2, to nl2)
                          in x2 >= x1 && x2 <  y1 || -- start of line2 within line1
                             y2 >  x1 && y2 <= y1 || -- end of line2 within line1
                             x2 <= x1 && y2 >= y1    -- line2 superset or equal to line1

mktestLine a b = Line a b "" "" ""  0 0 0 0
l1 = mktestLine (0,0) (10,0)
l2 = mktestLine (0,1) ( 0,4)
l3 = mktestLine (4,0) ( 8,0)
l4 = mktestLine (0,2) ( 0,5)
l5 = mktestLine (4,2) ( 7,2)

test_xaxis_intersect = assertBool       $checkIntersect l1 l3
test_xaxis_none      =(assertBool . not)$checkIntersect l1 l2
test_yaxis_intersect = assertBool       $checkIntersect l2 l4
test_yaxis_none      =(assertBool . not)$checkIntersect l2 l3
test_xaxis_none2     =(assertBool . not)$checkIntersect l1 l5

l9 = mktestLine (0,128) (128,128)
l10= mktestLine (128,128) (192,128)
test_from_example7   = (assertBool.not) $ checkIntersect l9 l10

main = htfMain htf_thisModulesTests

-- split lines in existing list with a splitter line (which we don't insert)
splitLines :: [Line] -> Line -> [Line]
splitLines [] _ = []
splitLines (l:ls) c =
    if   not (checkIntersect c l)
    then l : splitLines ls c
    else if   l == c
         then l:ls
         else splitLine l c ++ ls

test_splitlines   = assertEqual 3 (length (splitLines [l1] l3))
test_nosplitlines = assertEqual 1 (length (splitLines [l2] l1))

-- split a line by another, return the bits
splitLine :: Line -> Line -> [Line]
splitLine l cut = let  (a,b) = (from l, to l)
                       (c,d) = (from cut, to cut)
    in
    if   normalizeLine l == normalizeLine cut
    then [l]
    else let [e,f,g,h] = sort [a,b,c,d]
             nl1 = l { from = e, to = f }
             nl2 = l { from = f, to = g }
             nl3 = l { from = g, to = h }
         in map (if lineOrient l == LT then flipline else id) $
            filter (\m -> from m /= to m
                    && checkIntersect l m) [nl1, nl2, nl3]

test_splitline_self1 = assertEqual [l1] $ splitLine l1 (flipline l1)
test_splitline_self2 = assertEqual [l1] $ splitLine l1 l1
test_splitline_self3 = assertEqual [flipline l1] $ splitLine (flipline l1) l1

l11 = mktestLine (0,64)  (0,128)
l12 = mktestLine (0,192) (0,64)
l13 = mktestLine (0,0)   (0,128)

test_nosplit = assertEqual [l11] (splitLine l11 l12)

-- temporarily straight line!
-- limit the range of possible coordinate values to increase the likelyhood
-- of overlap for the default number of tests. Doom's actual range is
-- -32768 to +32767, but a more realistic upper limit is a range of about
-- 5000 units (MAP29)

instance Arbitrary Line where
    arbitrary = do
        vs <- replicateM 2 (choose (-2500, 2500))
        let [x,y] = vs
        return (mktestLine (x,0) (y,0))

prop_splitline_sameorient :: Line -> Line -> Bool
prop_splitline_sameorient x y = (>0) $ length $ filter (== (lineOrient x)) $ map lineOrient (splitLine x y)

test_splitline_sameorient1 = assertBool (prop_splitline_sameorient l12 l13)
test_splitline_sameorient2 = assertBool (prop_splitline_sameorient l13 l12)

-- catch case where the new lines from a split intersect existing lines
l6 = mktestLine (0,0) (0,2)
l7 = mktestLine (0,4) (0,6)
l8 = mktestLine (0,1) (0,5)
test_alldone = let split1 = splitLines [l6,l7] l8
                   repeat = split1 !! 2
                   split2 = splitLines split1 repeat
               in  assertEqual split1 split2
-- (probably worth writing a property, too)

-- again leveraging Ord tuple
instance Ord Line where
    x <= y = let (a,b,c,d) = (from x, to x, from y, to y) in (a,b) < (c,d)

normalizeLine l = if (from l) > (to l) then flipline l else l

test_rev_xaxis_intersect = assertBool       $checkIntersect l1 (flipline l3)
test_rev_xaxis_none      =(assertBool . not)$checkIntersect l1 (flipline l2)

test_rev_yaxis_intersect = assertBool       $checkIntersect l2 (flipline l4)
test_rev_yaxis_intersect2 = assertBool       $checkIntersect (flipline l2) (flipline l4)

test_rev_yaxis_none      =(assertBool . not)$checkIntersect l2 (flipline l3)
test_rev_xaxis_none2     =(assertBool . not)$checkIntersect l1 (flipline l5)
test_rev_splitlines   = assertEqual 3 (length (splitLines [l1] (flipline l3)))
test_rev_nosplitlines = assertEqual 1 (length (splitLines [l2] (flipline l1)))

test_rev_recurse = assertEqual 1 (length (splitLines [l1] (flipline l1)))

l14 = mktestLine (0,0)   (128,0)
l15 = mktestLine (256,0) (384,0)
l16 = mktestLine (-256,0) (512,0)
-- XXX need a test here (import example9)

-- XXX rename!
--          lines     cuts      lines
workbest :: [Line] -> [Line] -> [Line]
workbest [] _ = []
workbest ls [] = ls
workbest (l:ls) (c:cs) =
    let x = splitLine l  c
        y = workbest  x  cs
        z = workbest  ls (c:cs)
    in  y ++ z
