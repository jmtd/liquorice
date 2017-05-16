{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Wad ( WadLump(..)
           , wadLumpLength
           , wadLumpName
           , Linedef(..)
           , Sidedef(..)
           , WadMap(..)
           , mapWad2Wad
           , dumpWad

           , htf_thisModulesTests
           ) where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Binary
import Data.Binary.Put
import Test.Framework

import Liquorice

-- local cheats
str8 :: String -> L.ByteString
str8 s = let bit = take 8 s
             len = length bit
             pad = take (8 - len) (repeat '\0')
         in
            LC.pack $ bit ++ pad

putShort = putInt16le . fromIntegral
putInt   = putInt32le . fromIntegral

-- data structures to closely represent WAD lumps ----------------------------

data WadLump = WadLabel String
             | WadThings [Thing]   -- re-used from Liquorice
             | WadVertexes [Point] -- re-used from Liquorice
             | WadSectors [Sector] -- re-used from Liquorice (but might not want to)
             | WadLinedefs [Linedef]
             | WadSidedefs [Sidedef]
             | WadLump String L.ByteString deriving (Show)

wadLumpLength :: WadLump -> Int
wadLumpLength (WadLabel _)     = 0
wadLumpLength (WadThings ts)   = 10 * length ts
wadLumpLength (WadLump _ l)    = fromIntegral (LC.length l)
wadLumpLength (WadVertexes vs) = 4 * length vs
wadLumpLength (WadSectors ss)  = 26 * length ss
wadLumpLength (WadLinedefs ls) = 14 * length ls
wadLumpLength (WadSidedefs ss) = 30 * length ss

wadLumpName :: WadLump -> String
wadLumpName (WadLabel s) = s
wadLumpName (WadThings _) = "THINGS"
wadLumpName (WadVertexes _) = "VERTEXES"
wadLumpName (WadSectors _) = "SECTORS"
wadLumpName (WadLinedefs _)= "LINEDEFS"
wadLumpName (WadSidedefs _)= "SIDEDEFS"
wadLumpName (WadLump s _) = s

data Linedef = Linedef { ldFrom    :: Int -- all shorts
                       , ldTo      :: Int
                       , ldFlags   :: Int
                       , ldSpecial :: Int
                       , ldTag     :: Int
                       , ldFront   :: Int
                       , ldBack    :: Int
                       } deriving (Show, Eq)

data Sidedef = Sidedef { sdXoff   :: Int -- shorts or 8-byte strings
                       , sdYoff   :: Int
                       , sdUpper  :: String
                       , sdLower  :: String
                       , sdMid    :: String
                       , sdSector :: Int
                       } deriving (Show, Eq)

-- converting those data structures to ByteStrings ---------------------------

nope = error "not implemented"

instance Binary WadLump where
    get = nope
    put (WadLabel l)     = return ()
    put (WadLump _ d)    = put d -- XXX probably wrong! we want some literal put
    put (WadThings ts)   = mapM_ put ts
    put (WadVertexes vs) = mapM_ (\ (x,y) -> (putShort x >> putShort y)) vs
    put (WadSectors ss)  = mapM_ put ss
    put (WadLinedefs ls) = mapM_ put ls
    put (WadSidedefs ss) = mapM_ put ss

instance Binary Sector where
    get = nope
    put (Sector f c fflat cflat ll stype tag _) = do
        putShort f
        putShort c
        putLazyByteString (str8 fflat)
        putLazyByteString (str8 cflat)
        putShort ll
        putShort stype
        putShort tag

instance Binary Linedef where
    get = nope
    put (Linedef from to flags special tag front back) = do
       putShort from
       putShort to
       putShort flags
       putShort special
       putShort tag
       putShort front
       putShort back

instance Binary Sidedef where
    get = nope
    put (Sidedef xoff yoff upper lower mid sector) = do
        putShort xoff
        putShort yoff
        putLazyByteString (str8 upper)
        putLazyByteString (str8 lower)
        putLazyByteString (str8 mid)
        putShort sector

instance Binary Thing where
    get = nope
    put (Thing (x,y) angle ty flags) = do
        putShort x
        putShort y
        putShort angle
        putShort ty
        putShort flags

wadHeader :: Int -> Int -> L.ByteString
wadHeader numents diroffs = runPut $ do
    putStringUtf8 "PWAD"
    putInt numents
    putInt diroffs

-- convert a directory into bytes to write out
dumpWadDir :: [(Int,Int,String)] -> L.ByteString
dumpWadDir xs = L.concat (map dumpWadDir' xs) where
    dumpWadDir' :: (Int,Int,String) -> L.ByteString
    dumpWadDir' (offs,len,name) = runPut $ do
        putInt offs
        putInt len
        putLazyByteString (str8 name)

-- XXX: rename
dumpWad :: [WadLump] -> L.ByteString
dumpWad ws = let numents = length ws
                 diroffs = 12 + sum (map wadLumpLength ws)
                 header  = wadHeader numents diroffs
                 dir     = dumpWadDir (buildWadDir ws)
             in  L.concat (header : (map encode ws) ++ [dir])

buildWadDir :: [WadLump] -> [(Int,Int,String)] -- offs,len,name
buildWadDir ws = buildWadDir' 12 ws where
    buildWadDir' :: Int -> [WadLump] -> [(Int,Int,String)]
    buildWadDir' _ [] = []
    buildWadDir' offs (w:ws) = let len = wadLumpLength w
                               in (offs, len, wadLumpName w):(buildWadDir' (offs+len) ws)

-- similar to above; but exactly one map-related lump ------------------------

data WadMap = WadMap { mapLabel        :: String
                     , mapThings       :: [Thing]
                     , mapLinedefs     :: [Linedef]
                     , mapSidedefs     :: [Sidedef]
                     , mapVertexes     :: [Point]
                     , mapSectors      :: [Sector]
                     } deriving (Show, Eq)

mapWad2Wad :: WadMap -> [WadLump]
mapWad2Wad (WadMap label things lines sides vertexes sectors) =
    [ WadLabel label, WadThings things, WadLinedefs lines,
      WadSidedefs sides, WadVertexes vertexes, WadSectors sectors ]

buildMapWad :: WadMap -> L.ByteString
buildMapWad = dumpWad . mapWad2Wad

-- test data - raw PWAD read into a ByteString -------------------------------

main = htfMain htf_thisModulesTests
wad1 = LC.pack "PWAD\ACK\NUL\NUL\NUL\192\NUL\NUL\NUL \NUL@\NULZ\NUL\SOH\NUL\NUL\NUL\NUL\NUL\SOH\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\255\255\STX\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\SOH\NUL\255\255\SOH\NUL\STX\NUL\SOH\NUL\NUL\NUL\NUL\NUL\STX\NUL\255\255\NUL\NUL\NUL\NUL-\NUL\NUL\NUL\NUL\NUL\NUL\NUL-\NUL\NUL\NUL\NUL\NUL\NUL\NULZZWOLF3\NUL\NUL\NUL\NUL\NUL\NUL\NUL-\NUL\NUL\NUL\NUL\NUL\NUL\NUL-\NUL\NUL\NUL\NUL\NUL\NUL\NULZZWOLF2\NUL\NUL\NUL\NUL\NUL\NUL\NUL-\NUL\NUL\NUL\NUL\NUL\NUL\NUL-\NUL\NUL\NUL\NUL\NUL\NUL\NULZZWOLF1\NUL\NUL\NUL\128\NUL\128\NUL\NUL\NUL\NUL\NUL\NUL\NUL\128\NUL\NUL\NUL\128\NULFLAT23\NUL\NULF_SKY1\NUL\NUL\160\NUL\NUL\NUL\NUL\NUL\f\NUL\NUL\NUL\NUL\NUL\NUL\NULMAP01\NUL\NUL\NUL\f\NUL\NUL\NUL\n\NUL\NUL\NULTHINGS\NUL\NUL\SYN\NUL\NUL\NUL*\NUL\NUL\NULLINEDEFS@\NUL\NUL\NULZ\NUL\NUL\NULSIDEDEFS\154\NUL\NUL\NUL\f\NUL\NUL\NULVERTEXES\166\NUL\NUL\NUL\SUB\NUL\NUL\NULSECTORS\NUL"

wad8 :: [WadLump]
wad8 = [ WadLabel "MAP01"
       , WadThings [Thing (32,64) 90 1 0]
       , WadLinedefs [ Linedef 0 1 1 0 0 0 (-1)
                     , Linedef 2 0 1 0 0 1 (-1)
                     , Linedef 1 2 1 0 0 2 (-1) ]
       , WadSidedefs [ Sidedef 0 0 "-" "-" "ZZWOLF3" 0
                     , Sidedef 0 0 "-" "-" "ZZWOLF2" 0
                     , Sidedef 0 0 "-" "-" "ZZWOLF1" 0 ]
       , WadVertexes [(128,128), (0,0), (0,128)]
       , WadSectors [Sector 0 128 "FLAT23" "F_SKY1" 160 0 0 [] ]
       ]

test_equiv8 = assertEqual (dumpWad wad8) wad1

wad9 = WadMap { mapLabel    = "MAP01"
              , mapThings   = [Thing (32, 64) 90 1 0]
              , mapLinedefs = [ Linedef 0 1 1 0 0 0 (-1)
                              , Linedef 2 0 1 0 0 1 (-1)
                              , Linedef 1 2 1 0 0 2 (-1) ]
              , mapSidedefs = [ Sidedef 0 0 "-" "-" "ZZWOLF3" 0
                              , Sidedef 0 0 "-" "-" "ZZWOLF2" 0
                              , Sidedef 0 0 "-" "-" "ZZWOLF1" 0 ]
              , mapVertexes = [(128, 128), (0, 0), (0, 128)]
              , mapSectors  = [Sector 0 128 "FLAT23" "F_SKY1" 160 0 0 [] ]
              }

test_equiv9 = assertEqual ((dumpWad . mapWad2Wad) wad9) wad1
