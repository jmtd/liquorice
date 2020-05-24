{-
 - Shawn.hs - spell words using splices of the
 - texture 'SHAWN1'
 - inspired by Simon Howard's "uacletters"
 - https://github.com/fragglet/uacletters
 -}

module Shawn where

import System.IO
import Data.List
import Data.Char
import Data.Maybe

import Liquorice
import Liquorice.Monad
import Liquorice.Render

-- this is derived from a LUT calculated in Fraggle's
-- generate.py
alphabet =
      [ ('a', [(48, 20)])
      , ('c', [(85, 23)])
      , ('d', [(62, 3), (63, 3), (95, 5), (93, 1), (92, 1), (91, 1), (90, 1), (89, 1), (88, 1), (87, 1), (86, 1), (85, 1)])
      , ('i', [(61, 4), (63, 3)])
      , (' ', [(34, 5)])
      , ('j', [(20, 14)])
      , ('l', [(10, 14)])
      , ('o', [(85, 4), (90, 1), (89, 1), (91, 9), (93, 1), (92, 1), (91, 1), (90, 1), (89, 1), (88, 1), (87, 1), (86, 1), (85, 1)])
      , ('u', [(10, 24)])
      , ('w', [(11, 2), (14, 3), (18, 3), (23, 3), (28, 2), (14, 2), (18, 3), (23, 3), (27, 3), (31, 2)])
      ]
     -- , 'long_space': [(34, 13)]

suitable s = let avail = nub (map fst alphabet)
    in [] ==
    filter (\c -> not (c `elem` avail))
    (map toLower s)

shawnWord s = do
    let lines = concatMap (fromJust . (flip lookup) alphabet) (map toLower s)
    let width = sum (map snd lines)
    mid "SHAWN1"
    mapM_ (\(x,w) -> xoff x >> straight w) lines
    mid "STARTAN2"
    turnright
    straight 128
    turnright
    straight width
    turnright
    straight 128
    turnright
    rightsector 0 128 160
    step width 0

main = do
    f <- readFile "/usr/share/dict/words"
    let words = map (++"  ") (filter suitable (lines f))

    buildWad "shawn.wad" $ runWadL $ do
        place 32 32 thing
        mapM_ shawnWord words
