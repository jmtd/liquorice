-- offset overlapping sectors (tests line splitting, 2s lines)
module Main where

import Liquorice.Monad
import Liquorice.Render

main = buildWad "example8.wad" $ runWadL $ do
    box 128 128 0 128 160
    place 64 64 thing
    step 128 64
    box 128 128 0 128 160
