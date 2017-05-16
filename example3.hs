-- two disconnected boxes
module Main where

import Liquorice.Monad
import Render

main = buildWad "example3.wad" $ runWadL $ do
    box 128 128 0 128 160
    step 64 64
    thing
    step 128 (-64)
    box 128 128 0 128 160
