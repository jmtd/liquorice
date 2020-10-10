{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# OPTIONS_HADDOCK prune #-}

{-|
Module      : Liquorice
Description : Liquorice fundamental data-types.
Copyright   : © Jonathan Dowland, 2020
License     : GPL-3
Maintainer  : jon+hackage@dow.land
Stability   : experimental
Portability : POSIX

The Liquorice fundamental data-types.
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

    , angleToOrientation
    , orientationToAngle

    , start

    , htf_thisModulesTests
    ) where

import Test.Framework
import Data.Function ((&))
import Data.List (nub)

import Liquorice.Line

-- | Utility function, a wrapping alternative to `succ`.
next :: (Eq a, Bounded a, Enum a) => a -> a
next a = if a == maxBound then minBound else succ a

-- | Utility function, a wrapping alternative to `pred`.
prev :: (Eq a, Bounded a, Enum a) => a -> a
prev a = if a == minBound then maxBound else pred a

-- | Possible orientations for the Pen: The four cardinal directions.
data Orientation = North | East | South | West deriving (Show, Eq, Enum, Bounded)

-- | A `Context` holds the entirety of all geometry that has been defined at
-- a point in a program: properties of the Pen: `location` and `orientation`;
-- "Palette"-like properties of the Pen: the current thing Type, textures for
-- parts of `Line`s and `Sector`s, etc.;
-- Lists of geometric objects (`linedefs`,`sectors`,`things`).
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

                       -- for the pseudo-RNG
                       , seed :: Int

                       } deriving (Show, Eq)

-- | An approximation of Doom's Sector data-type, with unboxed fields.
-- Unlike Doom's Sector data structure, we maintain a list of lines
-- that belong to a given Sector.
data Sector = Sector { floorHeight         :: Int
                     , ceilHeight          :: Int
                     , floorFlat           :: String
                     , ceilFlat            :: String
                     , lightLevel          :: Int
                     , sectorType          :: Int
                     , sectorTag           :: Int
                     , sectorLines         :: [Line]
                     } deriving (Show, Eq)
-- XXX all are short (unsigned?) ints, 8 byte strings

-- | An approximation of Doom's Thing data-type, with unboxed fields.
data Thing = Thing { thingPos   :: Point
                   , thingAngle :: Int -- all shorts
                   , thingType  :: Int
                   , thingFlags :: Int
                   } deriving (Show, Eq)

angleToOrientation :: Int -> Orientation
angleToOrientation 0   = East
angleToOrientation 180 = South
angleToOrientation 270 = West
angleToOrientation _   = North

orientationToAngle :: Orientation -> Int
orientationToAngle East  = 0
orientationToAngle North = 90
orientationToAngle South = 180
orientationToAngle West  = 270

-- | An initial Context, from which to derive a new map. `Empty` lines
-- and `Sector` lists and sensible default values for "palette" parameters.
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
                , seed=0
                }
