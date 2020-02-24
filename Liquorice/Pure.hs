{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# OPTIONS_HADDOCK prune #-}

{-|
Module      : Liquorice.Pure
Description : Pure functions for building Liquorice programs
Copyright   : © Jonathan Dowland, 2020
License     : GPL-3
Maintainer  : jon+hackage@dow.land
Stability   : experimental
Portability : POSIX

These Pure functions are the basic Liquorice primitives for constructing
Liquorice programs that evaluate to Doom maps. Monadic equivalents are defined
in `Liquorice.Monad`. Most people may find those more convenient.
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

-- | Move the pen forwards and sideways by the supplied amounts.
step :: Int -> Int -> Context -> Context
step forward sideways c = c { location = newloc }
  where
    newloc = step' (location c) (orientation c)
    step' (x,y) North = (x+sideways, y+forward)
    step' (x,y) South = (x-sideways, y-forward)
    step' (x,y) East  = (x+forward, y-sideways)
    step' (x,y) West  = (x-forward, y+sideways)

-- | Define a line from the current `location` to a new one reached by
-- moving forwards and sideways by the supplied amounts.
draw :: Int -> Int -> Context -> Context
draw forward sideways oldc = let
    newc = step forward sideways oldc
    newline = Line (location oldc) (location newc) (paletteTop oldc)
        (paletteMid oldc) (paletteBot oldc) (paletteLineType oldc) (paletteLineTag oldc)
        (paletteXoff oldc) (paletteYoff oldc)
    in addLine newline newc

-- | Rotate the pen to the right.
turnright :: Context -> Context
turnright c = c { orientation=next (orientation c) }

-- | Rotate the pen to the left.
turnleft :: Context -> Context
turnleft c = c { orientation=prev (orientation c) }

-- | Rotate the pen 180°.
turnaround :: Context -> Context
turnaround c = c { orientation=(next.next.orientation) c }

-- | Define a new Sector on the right-hand side of the pen.
rightsector :: Int -> Int -> Int -> Context -> Context
rightsector f c l ctx = let s = Sector f c (paletteFloor ctx) (paletteCeil ctx) l (paletteSectorType ctx) (paletteSectorTag ctx) (linedefs ctx)
                            newsectors = s : sectors ctx
                         in ctx { sectors = newsectors, linedefs = [] }

-- | Define a new Sector on the left-hand side of the pen.
leftsector :: Int -> Int -> Int -> Context -> Context
leftsector f c l ctx = let ls = map flipline (linedefs ctx)
                           s = Sector f c (paletteFloor ctx) (paletteCeil ctx) l (paletteSectorType ctx) (paletteSectorTag ctx) ls
                           newsectors = s : sectors ctx
                        in ctx { sectors = newsectors, linedefs = [] }

-- | Define a new inner-Sector on the right-hand side of the pen.
innerrightsector :: Int -> Int -> Int -> Context -> Context
innerrightsector f c l ctx = let last    = head (sectors ctx)
                                 flipped = map flipline (linedefs ctx)
                                 newlast = last { sectorLines = sectorLines last ++ flipped }
                                 nctx    = ctx  { sectors = newlast : tail (sectors ctx) }
                             in  rightsector f c l nctx

-- | Define a new inner-Sector on the left-hand side of the pen.
innerleftsector :: Int -> Int -> Int -> Context -> Context
innerleftsector f c l ctx = let last    = head (sectors ctx)
                                newlast = last { sectorLines = sectorLines last ++ (linedefs ctx)}
                                nctx    = ctx  { sectors = newlast : tail (sectors ctx) }
                            in  leftsector f c l nctx

-- | Pop the last-defined `Sector` from the stack of defined `Sector`s and re-insert
-- it at the bottom. It remains defined, but the second-last-defined `Sector` is
-- promoted for subsequent operations that use the last-defined `Sector`.
popsector :: Context -> Context
popsector c = case length (sectors c) of
    0 -> c -- XXX catch error
    1 -> c -- XXX catch error
    _ -> let pop  = head (sectors c)
             news = tail (sectors c) ++ [pop]
         in c { sectors = news }

-- XXX TESTS!!!

-- | Define a new `Thing` at the current pen location.
thing :: Context -> Context
thing c = let newthing = Thing (location c) angle (curThingType c) 7 -- all skills
              angle = case orientation c of
                  North -> 90
                  East  -> 0
                  South -> 270
                  West  -> 180
          in c { things = newthing : things c }

-- | Set the mid-texture value for future lines.
mid :: String -> Context -> Context
mid s c = c { paletteMid = s }

-- | Set the lower-texture value for future lines.
lower :: String -> Context -> Context
lower s c = c { paletteBot = s }

-- | Set the texture x-offset value for future lines.
xoff :: Int -> Context -> Context
xoff x c = c { paletteXoff = x }

-- | Set the texture y-offset value for future lines.
yoff :: Int -> Context -> Context
yoff y c = c { paletteYoff = y }

-- | Set the upper-texture value for future lines.
upper :: String -> Context -> Context
upper s c = c { paletteTop = s }

-- | Set the Sector floor texture for future Sectors.
floorflat :: String -> Context -> Context
floorflat s c = c { paletteFloor = s }
-- XXX: rename/harmonize with ceil

-- | Set the Sector ceiling texture for future Sectors.
ceil :: String -> Context -> Context
ceil s c = c { paletteCeil = s }

-- | Set the type and tag values for future defined `Line`s.
linetype :: Int -> Int -> Context -> Context
linetype ty tag c = c { paletteLineType = ty, paletteLineTag = tag }

-- | Set the type and tag values for future defined `Sector`s.
sectortype :: Int -> Int -> Context -> Context
sectortype ty tag c = c { paletteSectorType = ty, paletteSectorTag = tag }

-- | Set the type of future-defined `Thing`s.
setthing :: Int -> Context -> Context
setthing s c = c { curThingType = s }

-- | Set the name of the current map.
mapname :: String -> Context -> Context
mapname s c = c { mapName = s }

-- | Evaluate `fn` at a `location` offset of (x,y) from the current pen
-- location and then move the pen location back by the same relative amount.
place :: Int -> Int -> (Context -> Context) -> Context -> Context
place x y fn c = c & step x y
                    & fn
                    & step (-1 * x) (-1 * y)

test_place_pos = assertEqual (location a) (location b) where
    a = start
    b = place 64 64 thing start

-- | Evaluate `fn` and then re-define the `location` to the value it was
-- prior to evaluation.
pushpop :: (Context -> Context) -> Context -> Context
pushpop fn c = (fn c) { location = location c }

test_pushpop_pos = assertEqual (location a) (location b) where
    a = start
    b = pushpop (thing . step 64 64) start

test_no_things = assertEqual 0 (length (things start))
test_one_thing = assertEqual 1 (length (things (start & thing)))

-- | Evaluate `f` twice.
twice :: (Context -> Context) -> Context -> Context
twice f c = iterate f c !! 2

-- | Evaluate `f` four times.
quad :: (Context -> Context) -> Context -> Context
quad f c = iterate f c !! 4

-- | Define a straight `Line` of length n from the current pen position along the
-- current orientation.
straight n = draw n 0

box' :: Int -> Int -> Context -> Context
box' h w = twice (\c -> c
                 & straight h
                 & turnright
                 & straight w
                 & turnright)

-- | Define a rectangular `Sector` of the supplied size and properties.
box :: Int -> Int -> Int -> Int -> Int -> Context -> Context
box h w f ceil l c = c
     & box' h w
     & rightsector f ceil l

-- | Define a rectangular inner-`Sector` of the supplied size and properties,
-- parented to the last-drawn Sector.
ibox :: Int -> Int -> Int -> Int -> Int -> Context -> Context
ibox h w f ceil l c = c
     & box' h w
     & innerrightsector f ceil l

-- | Check intersections against all existing lines
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
