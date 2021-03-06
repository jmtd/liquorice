{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE TemplateHaskell #-}

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
    ( pureFns

    , addLine
    , box
    , ceil
    , draw
    , floorflat
    , ibox
    , innerleftsector
    , innerrightsector
    , left
    , leftsector
    , linetype
    , lower
    , mapname
    , mid
    , place
    , popsector
    , pushpop
    , quad
    , rightsector
    , right
    , sectortype
    , setthing
    , step
    , step'
    , straight
    , thing
    , triple
    , turnaround
    , turnleft
    , turnright
    , twice
    , upper
    , withXoff
    , xoff
    , yoff

    , deathmatchstart
    , boxofrockets
    , rocket
    , stimpak
    , greenarmor
    , rocketlauncher
    , lostsoul
    , chaingun
    , healthpotion
    , spiritarmor
    , cyberdemon
    , shortredfirestick
    , player1start
    , player2start
    , player3start
    , player4start
    , teleportdest
    , candle
    , soulsphere

    , setseed
    , rand

    , htf_thisModulesTests
    ) where

import Test.Framework
import Data.Function ((&))
import Data.List (nub)

import Liquorice
import Liquorice.Line

main = htfMain htf_thisModulesTests

pureFns = [ 'addLine
          , 'box
          , 'ceil
          , 'draw
          , 'floorflat
          , 'ibox
          , 'innerleftsector
          , 'innerrightsector
          , 'left
          , 'leftsector
          , 'linetype
          , 'lower
          , 'mapname
          , 'mid
          , 'place
          , 'popsector
          , 'pushpop
          , 'quad
          , 'rightsector
          , 'right
          , 'sectortype
          , 'setthing
          , 'step
          , 'straight
          , 'thing
          , 'triple
          , 'turnaround
          , 'turnleft
          , 'turnright
          , 'twice
          , 'upper
          , 'withXoff
          , 'xoff
          , 'yoff
          , 'deathmatchstart
          , 'boxofrockets
          , 'rocket
          , 'stimpak
          , 'greenarmor
          , 'rocketlauncher
          , 'lostsoul
          , 'chaingun
          , 'healthpotion
          , 'spiritarmor
          , 'cyberdemon
          , 'shortredfirestick
          , 'player1start
          , 'player2start
          , 'player3start
          , 'player4start
          , 'teleportdest
          , 'candle
          , 'soulsphere
          , 'setseed
          ]

-- | Move the pen forwards and sideways by the supplied amounts.
step :: Int -> Int -> Context -> Context
step forward sideways c = c { location = newloc }
  where
    newloc = step' (location c) (orientation c) forward sideways

-- | Given a location and orientation, return the adjusted delta
-- to apply to global coordinates in order to move forward and
-- sideways by the provided amounts
step' (x,y) North forward sideways = (x+sideways, y+forward)
step' (x,y) South forward sideways = (x-sideways, y-forward)
step' (x,y) East  forward sideways = (x+forward, y-sideways)
step' (x,y) West  forward sideways = (x-forward, y+sideways)

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

-- | draw a line to the right
right :: Int -> Context -> Context
right x = straight x . turnright

-- | draw a line to the left
left :: Int -> Context -> Context
left x = straight x . turnleft

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
              angle = orientationToAngle (orientation c)
          in c { things = newthing : things c, seed = seed c + 1 }

-- | Set the mid-texture value for future lines.
mid :: String -> Context -> Context
mid s c = c { paletteMid = s }

-- | Set the lower-texture value for future lines.
lower :: String -> Context -> Context
lower s c = c { paletteBot = s }

-- | Perform the supplied actions with `paletteXoff` set to the supplied value,
-- then reset `paletteXoff`.
withXoff :: Int -> (Context -> Context) -> Context -> Context
withXoff x fn c = c & xoff x & fn & xoff (paletteXoff c)

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

-- | Set the type of future-defined `Thing`s. We accept Integer here
-- to match Haskell's defaulting of numeric constants.
setthing :: Integer -> Context -> Context
setthing s c = c { curThingType = fromIntegral s }

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

-- | Evaluate `f` three times.
triple :: (Context -> Context) -> Context -> Context
triple f c = iterate f c !! 3

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
    in c { sectors = news, linedefs = linedefs c ++ newlines, seed = seed c + 1 }

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

deathmatchstart   = setthing 11
boxofrockets      = setthing 2046
rocket            = setthing 2010
stimpak           = setthing 2011
greenarmor        = setthing 2018
rocketlauncher    = setthing 2003
lostsoul          = setthing 3006
chaingun          = setthing 2002
healthpotion      = setthing 2014
spiritarmor       = setthing 2015
cyberdemon        = setthing 16
shortredfirestick = setthing 57
player1start      = setthing 1
player2start      = setthing 2
player3start      = setthing 3
player4start      = setthing 4
teleportdest      = setthing 14
candle            = setthing 34
soulsphere        = setthing 2013

-- | set the pseudo-RNG seed to the supplied number.
setseed :: Int -> Context -> Context
setseed i c = c { seed = i }

-- a pool of shuffled Ints, values 0-255
-- pool = System.Random.Shuffle.shuffleM ([0..255] :: [Int])
pool :: [Int]
pool = [185, 253, 44, 94, 157, 115, 143, 196, 16, 208, 146, 53, 81, 45,
    224, 92, 141, 176, 0, 51, 161, 83, 216, 237, 86, 156, 96, 243, 254, 192,
    153, 31, 60, 134, 218, 177, 242, 147, 206, 36, 113, 107, 59, 111, 80, 190,
    245, 124, 142, 171, 5, 189, 75, 85, 211, 163, 37, 165, 119, 175, 56, 123,
    210, 118, 12, 122, 90, 207, 127, 30, 57, 155, 73, 120, 172, 125, 38, 200,
    89, 40, 78, 68, 198, 221, 234, 230, 47, 193, 204, 50, 194, 246, 233, 209,
    248, 43, 250, 169, 227, 187, 84, 24, 251, 222, 199, 137, 167, 6, 116, 173,
    131, 22, 34, 13, 17, 226, 64, 203, 72, 126, 74, 99, 150, 27, 148, 63, 133,
    58, 100, 158, 255, 11, 184, 106, 66, 117, 4, 247, 239, 2, 98, 109, 235,
    101, 105, 205, 19, 93, 108, 95, 195, 9, 139, 128, 102, 67, 186, 183, 149,
    140, 112, 18, 91, 79, 215, 252, 46, 130, 170, 10, 217, 240, 82, 136, 88,
    15, 191, 7, 166, 159, 97, 214, 77, 28, 174, 135, 249, 212, 69, 35, 110, 52,
    65, 32, 39, 231, 244, 1, 138, 225, 164, 33, 103, 114, 228, 241, 179, 48,
    220, 178, 201, 229, 55, 70, 154, 20, 182, 87, 8, 223, 129, 25, 42, 41, 121,
    160, 197, 76, 26, 188, 213, 168, 29, 236, 219, 71, 152, 104, 151, 14, 54,
    180, 202, 132, 181, 238, 162, 61, 232, 144, 3, 145, 62, 49, 23, 21]

-- | pick a random number from a pool. Use the Context's current
-- seed value as an index. The seed value is not changed, so successive
-- calls will return the same number.
rand :: Context -> Int
rand c = pool !! (seed c `mod` length pool)
