{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-
    the Liquorice pure functions, data type declarations, basic
    atomic functions, and sample (pure) programs
 -}
module Liquorice
    (
      Context(..)
    , Point
    , Orientation(..)
    , prev
    , next
    , Thing(..)
    , Sector(..)

    , start
    , addLine

    , htf_thisModulesTests
    ) where

import Test.Framework
import Data.Function ((&))
import Data.List (nub)

import Line

next :: (Eq a, Bounded a, Enum a) => a -> a
next a = if a == maxBound then minBound else succ a
prev :: (Eq a, Bounded a, Enum a) => a -> a
prev a = if a == minBound then maxBound else pred a

data Orientation = North | East | South | West deriving (Show, Eq, Enum, Bounded)

data Context = Context { location :: Point
                       , orientation :: Orientation

                       , linedefs :: [Line]
                       , sectors :: [Sector]
                       , things :: [Thing]

                       -- "palette": thing type; texture choices, etc
                       , curThingType  :: Int
                       , paletteTop :: String
                       , paletteMid :: String
                       , paletteBot :: String
                       , paletteFloor :: String
                       , paletteCeil :: String
                       , paletteLineType :: Int
                       , paletteLineTag  :: Int
                       , paletteSectorType :: Int
                       , paletteSectorTag  :: Int
                       , paletteXoff :: Int
                       , paletteYoff :: Int
                       , mapName :: String

                       } deriving (Show, Eq)

-- XXX all are short (unsigned?) ints, 8 byte strings
data Sector = Sector { floorHeight         :: Int
                     , ceilHeight          :: Int
                     , floorFlat           :: String
                     , ceilFlat            :: String
                     , lightLevel          :: Int
                     , sectorType          :: Int
                     , sectorTag           :: Int
                     , sectorLines         :: [Line]
                     } deriving (Show, Eq)

data Thing = Thing { thingPos   :: Point
                   , thingAngle :: Int -- all shorts
                   , thingType  :: Int
                   , thingFlags :: Int
                   } deriving (Show, Eq)

start :: Context
start = Context { location=(0,0)
                , orientation=North
                , linedefs=[]
                , sectors=[]
                , curThingType=1
                , things=[]
                , paletteTop="STARTAN3"
                , paletteMid="STARTAN3"
                , paletteBot="STARTAN3"
                , paletteFloor="FLAT23"
                , paletteCeil="F_SKY1"
                , paletteLineType=0
                , paletteLineTag=0
                , paletteSectorType=0
                , paletteSectorTag=0
                , mapName="MAP01"
                , paletteXoff=0
                , paletteYoff=0
                }

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

test_example9 = let pre  = addLine (Line (0,0) (0,128) "" "" "" 0 0 0 0) start
                    post = addLine (Line (0,64) (0,192) "" "" "" 0 0 0 0) pre
                in  assertEqual 3 (length (linedefs post))


main = htfMain htf_thisModulesTests
