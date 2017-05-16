-- an inner sector and 2s lines
module Main where

import Liquorice.Monad
import Render

main = buildWad "example6.wad" $ runWadL $ do
    box 256 256 0 128 160
    place 32 32 $ do
        thing
        quad $ do
          straight 64
          turnright
        innerrightsector 16 128 160
