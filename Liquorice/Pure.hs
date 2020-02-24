
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-
    basic HWaDC primitives, pure versions
 -}
module Liquorice.Pure
    ( draw
    , rightsector
    , step
    , turnright
    , turnleft
    , turnaround
    , innerrightsector
    , innerleftsector
    , leftsector
    , popsector
    , thing
    , mid
    , upper
    , lower
    , xoff
    , yoff
    , floorflat
    , ceil
    , linetype
    , sectortype
    , setthing
    , mapname
    , box
    , ibox
    , pushpop
    , place
    , addLine

    , htf_thisModulesTests
    ) where

import Test.Framework
import Data.Function ((&))
import Data.List (nub)

import Liquorice
import Liquorice.Line

main = htfMain htf_thisModulesTests

step :: Int -> Int -> Context -> Context
step forward sideways c = c { location = newloc }
  where
    newloc = step' (location c) (orientation c)
    step' (x,y) North = (x+sideways, y+forward)
    step' (x,y) South = (x-sideways, y-forward)
    step' (x,y) East  = (x+forward, y-sideways)
    step' (x,y) West  = (x-forward, y+sideways)

draw :: Int -> Int -> Context -> Context
draw forward sideways oldc = let
    newc = step forward sideways oldc
    newline = Line (location oldc) (location newc) (paletteTop oldc)
        (paletteMid oldc) (paletteBot oldc) (paletteLineType oldc) (paletteLineTag oldc)
        (paletteXoff oldc) (paletteYoff oldc)
    in addLine newline newc

turnright :: Context -> Context
turnright c = c { orientation=next (orientation c) }

turnleft :: Context -> Context
turnleft c = c { orientation=prev (orientation c) }

turnaround :: Context -> Context
turnaround c = c { orientation=(next.next.orientation) c }

rightsector :: Int -> Int -> Int -> Context -> Context
rightsector f c l ctx = let s = Sector f c (paletteFloor ctx) (paletteCeil ctx) l (paletteSectorType ctx) (paletteSectorTag ctx) (linedefs ctx)
                            newsectors = s : sectors ctx
                         in ctx { sectors = newsectors, linedefs = [] }

leftsector :: Int -> Int -> Int -> Context -> Context
leftsector f c l ctx = let ls = map flipline (linedefs ctx)
                           s = Sector f c (paletteFloor ctx) (paletteCeil ctx) l (paletteSectorType ctx) (paletteSectorTag ctx) ls
                           newsectors = s : sectors ctx
                        in ctx { sectors = newsectors, linedefs = [] }

innerrightsector :: Int -> Int -> Int -> Context -> Context
innerrightsector f c l ctx = let last    = head (sectors ctx)
                                 flipped = map flipline (linedefs ctx)
                                 newlast = last { sectorLines = sectorLines last ++ flipped }
                                 nctx    = ctx  { sectors = newlast : tail (sectors ctx) }
                             in  rightsector f c l nctx

innerleftsector :: Int -> Int -> Int -> Context -> Context
innerleftsector f c l ctx = let last    = head (sectors ctx)
                                newlast = last { sectorLines = sectorLines last ++ (linedefs ctx)}
                                nctx    = ctx  { sectors = newlast : tail (sectors ctx) }
                            in  leftsector f c l nctx

popsector :: Context -> Context
popsector c = case length (sectors c) of
    0 -> c -- XXX catch error
    1 -> c -- XXX catch error
    _ -> let pop  = head (sectors c)
             news = tail (sectors c) ++ [pop]
         in c { sectors = news }

-- XXX TESTS!!!

thing :: Context -> Context
thing c = let newthing = Thing (location c) angle (curThingType c) 7 -- all skills
              angle = case orientation c of
                  North -> 90
                  East  -> 0
                  South -> 270
                  West  -> 180
          in c { things = newthing : things c }


mid :: String -> Context -> Context
mid s c = c { paletteMid = s }

lower :: String -> Context -> Context
lower s c = c { paletteBot = s }

xoff :: Int -> Context -> Context
xoff x c = c { paletteXoff = x }

yoff :: Int -> Context -> Context
yoff y c = c { paletteYoff = y }

upper :: String -> Context -> Context
upper s c = c { paletteTop = s }

-- XXX: rename/harmonize with ceil
floorflat :: String -> Context -> Context
floorflat s c = c { paletteFloor = s }

ceil :: String -> Context -> Context
ceil s c = c { paletteCeil = s }

linetype :: Int -> Int -> Context -> Context
linetype ty tag c = c { paletteLineType = ty, paletteLineTag = tag }

sectortype :: Int -> Int -> Context -> Context
sectortype ty tag c = c { paletteSectorType = ty, paletteSectorTag = tag }

setthing :: Int -> Context -> Context
setthing s c = c { curThingType = s }

mapname :: String -> Context -> Context
mapname s c = c { mapName = s }

place :: Int -> Int -> (Context -> Context) -> Context -> Context
place x y fn c = c & step x y
                    & fn
                    & step (-1 * x) (-1 * y)

test_place_pos = assertEqual (location a) (location b) where
    a = start
    b = place 64 64 thing start

pushpop :: (Context -> Context) -> Context -> Context
pushpop fn c = (fn c) { location = location c }

test_pushpop_pos = assertEqual (location a) (location b) where
    a = start
    b = pushpop (thing . step 64 64) start

test_no_things = assertEqual 0 (length (things start))
test_one_thing = assertEqual 1 (length (things (start & thing)))

twice :: (Context -> Context) -> Context -> Context
twice f c = iterate f c !! 2

quad :: (Context -> Context) -> Context -> Context
quad f c = iterate f c !! 4

straight n = draw n 0

box' :: Int -> Int -> Context -> Context
box' h w = twice (\c -> c
                 & straight h
                 & turnright
                 & straight w
                 & turnright)

box :: Int -> Int -> Int -> Int -> Int -> Context -> Context
box h w f ceil l c = c
     & box' h w
     & rightsector f ceil l

ibox :: Int -> Int -> Int -> Int -> Int -> Context -> Context
ibox h w f ceil l c = c
     & box' h w
     & innerrightsector f ceil l

-- check intersections against all existing lines
addLine :: Line -> Context -> Context
addLine l c = let
    news       = map (\s-> s { sectorLines = splitLines (sectorLines s) l }) (sectors c)
    alllines   = linedefs c ++ concatMap sectorLines news
    intersects = filter (checkIntersect l) alllines
    newlines   = if length intersects > 0
                 then workbest [l] intersects
                 else [l]
    in c { sectors = news, linedefs = linedefs c ++ newlines }

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
