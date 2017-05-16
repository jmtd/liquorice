-- as example3, but use place
module Main where

import Liquorice.Monad
import Render

main = buildWad "example5.wad" $ runWadL $ do
    box 128 128 0 128 160
    place 64 64 thing
    step 192 0
    box 128 128 0 128 160
