-- attempt a blockmap

{- start with a 1D blockmap, have we claimed the block ahead already

    let's just use a simple list of Ints...

-}
module Main where

import Liquorice
import Liquorice.Monad
import Liquorice.Render
import Control.Monad.State.Lazy

type Block = Int
type Blockmap = [Block]

checkblock :: Block -> Blockmap -> Bool
checkblock = elem

blocksize = 128

moveToBlock :: Int -> State Context ()
moveToBlock block = do
    old <- get
    put old { location = (0,block * blocksize) }

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

    newmap1 <- foo 0 blockmap (box 128 128 0 128 160)
    newmap2 <- foo 1 newmap1 (box 128 128 0 128 160)
    --newmap3 <- foo 1 newmap2 (box 128 128 0 128 160) -- errors, as it should
    return ()
