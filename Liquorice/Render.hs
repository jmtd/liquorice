{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# OPTIONS_HADDOCK prune #-}

{-|
Module      : Liquorice.Render
Description : High-level functions for rendering Liquorice programs to PWAD files.
Copyright   : © Jonathan Dowland, 2020
License     : GPL-3
Maintainer  : jon+hackage@dow.land
Stability   : experimental
Portability : POSIX

This module exposes a high-level function `buildWad` that can be used to evaluate
a Liquorice program, convert the resulting Context into Doom data structures and
write out a Doom-format PWAD file.
-}

module Liquorice.Render ( buildWad
                        , htf_thisModulesTests
                        ) where

import qualified Data.ByteString.Lazy as L
import Test.Framework
import Data.List (elemIndex)
import Data.Function ((&))

import Liquorice
import Liquorice.Pure
import Liquorice.Line
import Doom.Wad

-- utility stuff -------------------------------------------------------------

addUnique :: Eq a => [a] -> a -> (Int,[a])
addUnique vs v = case elemIndex v vs of
                     Nothing -> (length vs, vs++[v])
                     Just l  -> (l, vs)

prop_AddUniqueWorks :: [Int] -> Int -> Bool
prop_AddUniqueWorks vs v = let (index,vs2) = addUnique vs v
                           in v == vs2 !! index

-- converting Liquorice structures into WAD structures ---------------------------

convertWadL :: Context -> WadMap
convertWadL c = let baseMap = WadMap (mapName c) (things c) [] [] [] []
                in  convertWadL' (sectors c) 0 baseMap
                where
    convertWadL' :: [Sector] -> Int -> WadMap -> WadMap
    convertWadL' [] _ m = m
    convertWadL' (s:ss) n m = let newS = s { sectorLines = [] } -- a hack
                                  m2   = convertLines (sectorLines s) n m
                                  m3   = m2 { mapSectors = mapSectors m2 ++ [newS] }
                              in  convertWadL' ss (n+1) m3
        where
        convertLines :: [Line] -> Int -> WadMap -> WadMap
        convertLines [] _ m = m
        convertLines (l:ls) n m = convertLines ls n (line2Def l n m)

-- XXX: detect existing line (backwards possibly); if present modify to 2 sided
-- (means generating two new sidedefs)

partitionLines :: Int -> Int -> WadMap -> [[Linedef]]
partitionLines vfrom vto m = let existingLines = mapLinedefs m
                                 pred   = \l -> (ldFrom l, ldTo l) `elem` [(vfrom,vto), (vto,vfrom)]
                                 before = takeWhile (not . pred) existingLines
                                 after  = drop (length before) existingLines
                             in  [before,after]
-- output if there's a match: [[all linedefs before], [match, all after]]
-- output if there isn't:  [[all linedefs], []]
-- (need to avoid tail in "after" for the [] case)

line2Def :: Line -> Int -> WadMap -> WadMap
line2Def l secno m = let
    (vfrom, vs1)   = addUnique (mapVertexes m) (from l)
    (vto,   vs2)   = addUnique vs1 (to l)
    [before,after] = partitionLines vfrom vto m

    in case after of
        -- old case, no overlap, single-sided
        [] -> let
            sidedef   = Sidedef (lineXoff l) (lineYoff l) (lineTop l) (lineBot l) (lineMid l) secno
            (sno,sd2) = addUnique (mapSidedefs m) sidedef
            newline   = Linedef vfrom vto 1 (lineType l) (lineTag l) sno (-1)
            newlines  = mapLinedefs m ++ [newline]
            in m { mapVertexes = vs2, mapSidedefs = sd2, mapLinedefs = newlines }

        -- new case, 2-sided, replace existing line definition
        (oldline:ls) -> let
            oldside   = (mapSidedefs m) !! (ldFront oldline)
            (so1,sd1) = addUnique (mapSidedefs m) oldside { sdMid = "-" }
            (so2,sd2) = addUnique sd1 (Sidedef (lineXoff l) (lineYoff l) (lineTop l) (lineBot l) "-" secno)
            newline   = oldline { ldFlags = 4, ldFront = so1, ldBack = so2 }
            newlines  = before ++ newline:ls
            in m { mapVertexes = vs2, mapSidedefs = sd2, mapLinedefs = newlines }

-- | Convert the geometry described by the supplied Context into Doom-format
-- structures and write them out as a PWAD file to the supplied FilePath.
buildWad outfile wadcsrc =
    L.writeFile outfile $ (dumpWad . mapWad2Wad . convertWadL) wadcsrc

-- tests ---------------------------------------------------------------------

-- test 1: single triangular sector, unique texture per line
wad1 = WadMap { mapLabel    = "MAP01"
              , mapThings   = [Thing (32, 64) 90 1 7]
              , mapLinedefs = [ Linedef 0 1 1 0 0 0 (-1)
                              , Linedef 1 2 1 0 0 1 (-1)
                              , Linedef 2 0 1 0 0 2 (-1)
                              ]
              , mapSidedefs = [ Sidedef 0 0 "STARTAN3" "STARTAN3" "ZZWOLF1" 0
                              , Sidedef 0 0 "STARTAN3" "STARTAN3" "ZZWOLF2" 0
                              , Sidedef 0 0 "STARTAN3" "STARTAN3" "ZZWOLF3" 0 ]
              , mapVertexes = [(0, 0), (0, 128), (128, 128)]
              , mapSectors  = [Sector 0 128 "FLAT23" "F_SKY1" 160 0 0 [] ]
              }

example1 = start
    & mid "ZZWOLF1"
    & draw 128 0
    & mid "ZZWOLF2"
    & draw 0 128
    & turnaround
    & mid "ZZWOLF3"
    & draw 128 128
    & rightsector 0 128 160
    & turnaround
    & step 64 32
    & thing

test_equiv1 = assertEqual (convertWadL example1) wad1

-- test 2: two (disconnected) sectors
example3 = start
    & box 128 128 0 128 160
    & step 64 64
    & thing
    & step 128 (-64)
    & box 128 128 0 128 160

wad3 = WadMap { mapLabel    = "MAP01"
              , mapThings   = [Thing (64, 64) 90 1 0]
              , mapLinedefs = [ Linedef 0 1 1 0 0 0 (-1)
                              , Linedef 2 0 1 0 0 0 (-1)
                              , Linedef 3 2 1 0 0 0 (-1)
                              , Linedef 1 3 1 0 0 0 (-1)
                              , Linedef 4 5 1 0 0 1 (-1)
                              , Linedef 6 4 1 0 0 1 (-1)
                              , Linedef 7 6 1 0 0 1 (-1)
                              , Linedef 5 7 1 0 0 1 (-1)
                              ]
              , mapSidedefs = [ Sidedef 0 0 "STARTAN3" "STARTAN3" "STARTAN3" 0
                              , Sidedef 0 0 "STARTAN3" "STARTAN3" "STARTAN3" 1 ]
              , mapVertexes = [(128, 192), (0, 192), (128, 320), (0, 320),
                               (128, 0), (0, 0), (128, 128), (0, 128)]

              , mapSectors  = [ Sector 0 128 "FLAT23" "F_SKY1" 160 0 0 []
                              , Sector 0 128 "FLAT23" "F_SKY1" 160 0 0 []
                              ]
              }

--test_equiv3 = assertEqual (convertWadL example3) wad3

-- test 3: two adjacent sectors (2s line) (based on example4)

example4 = start
    & box 128 128 0 128 160
    & pushpop (\c -> c & step 64 64 & thing)
    & step 128 0
    & box 128 128 0 128 160

wad4 = WadMap { mapLabel    = "MAP01"
              , mapThings   = [Thing (64, 64) 90 1 7]
              , mapLinedefs = [ Linedef 0 1 1 0 0 0 (-1)
                              , Linedef 1 2 1 0 0 0 (-1)
                              , Linedef 2 3 1 0 0 0 (-1)
                              , Linedef 3 0 4 0 0 2 3
                              , Linedef 4 0 1 0 0 1 (-1)
                              , Linedef 3 5 1 0 0 1 (-1)
                              , Linedef 5 4 1 0 0 1 (-1) ]
              , mapSidedefs = [ Sidedef 0 0 "STARTAN3" "STARTAN3" "STARTAN3" 0
                              , Sidedef 0 0 "STARTAN3" "STARTAN3" "STARTAN3" 1
                              , Sidedef 0 0 "STARTAN3" "STARTAN3" "-" 0
                              , Sidedef 0 0 "STARTAN3" "STARTAN3" "-" 1 ]
              , mapVertexes = [(0,128),(0,256),(128,256),(128,128),(0,0),(128,0)]

              , mapSectors  = [ Sector 0 128 "FLAT23" "F_SKY1" 160 0 0 []
                              , Sector 0 128 "FLAT23" "F_SKY1" 160 0 0 [] ]
              }

test_equiv4 = assertEqual (convertWadL example4) wad4

main = htfMain htf_thisModulesTests
