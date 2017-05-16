-- simple box example; default line textures
module Main where

import Liquorice.Monad
import Render

example = runWadL $ do
    box 128 128 0 128 160
    step 64 64
    thing

main = buildWad "example2.wad" example
