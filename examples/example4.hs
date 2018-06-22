-- as example3, but use pushpop
module Main where

import Liquorice.Monad
import Liquorice.Render

main = buildWad "example4.wad" $ runWadL $ do
    box 128 128 0 128 160
    pushpop $ do
        step 64 64
        thing
    step 192 0
    box 128 128 0 128 160
