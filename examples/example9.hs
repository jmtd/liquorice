-- basic blockmap. Routines to test and assign blocks in a X/Y plane
-- this will be more interesting when we've tackled randomness

module Main where

import Liquorice
import Liquorice.Monad
import Liquorice.Render
import Control.Monad.State.Lazy

type Block = (Int,Int)
type Blockmap = [Block]
blocksize = 128

checkblock :: Block -> Blockmap -> Bool
checkblock = elem

moveToBlock :: Block -> State Context ()
moveToBlock (x,y) = do
    old <- get
    put old { location = (x*blocksize, y*blocksize) }

foo targetblock blockmap x =
    if   checkblock targetblock blockmap
    then error "block already assigned"
    else let newblockmap = targetblock:blockmap
         in do
             moveToBlock targetblock
             x
             return newblockmap

main = buildWad "example9.wad" $ runWadL $ do
    let blockmap = []
    place 64 64 thing

    newmap1 <- foo (0,0) blockmap (box 128 128 0 128 160)
    newmap2 <- foo (0,1) newmap1 (box 128 128 0 128 160)
    return ()
