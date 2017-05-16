-- offset overlapping sectors (tests line splitting, 2s lines)
-- overlapping lines same direction (also test leftsector)
module Main where

import Liquorice.Monad
import Render

main = buildWad "example8.wad" $ runWadL $ do
    place 64 64 thing
    box 128 128 0 128 160
    step 128 64

    -- ensure the overlapping lines have the same orientation
    turnright
    draw 128 0
    triple $ do
        turnleft
        draw 128 0
    leftsector 16 (128+16) 100
