-- A version of example8.hs written using the Pure interface,
-- for comparison.
module Main where

import Liquorice
import Liquorice.Pure
import Liquorice.Render
import Data.Function ((&))

main = buildWad "example8b.wad" $
    start
    & place 64 64 thing
    & box 128 128 0 128 160
    & step 128 64

    -- ensure the overlapping lines have the same orientation
    & turnright
    & draw 128 0
    & triple (\c -> c & turnleft & draw 128 0)
    & leftsector 16 (128+16) 100
