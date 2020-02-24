{-# OPTIONS_GHC -F -pgmF htfpp #-}
{- We can't use Haddock "prune" here until we document all the wrapped
 - Pure functions -}

{-|
Module      : Liquorice.Monad
Description : Liquorice functions under the `State` Monad.
Copyright   : © Jonathan Dowland, 2020
License     : GPL-3
Maintainer  : jon+hackage@dow.land
Stability   : experimental
Portability : POSIX

Core `Liquorice` functions for building maps. These are all under the
`State` Monad, the state which is passed around is the `Context` being
operated on.

Many of these functions are wrapped versions of those in `Liquorice.Pure`.
-}
module Liquorice.Monad
(     twice
    , triple
    , quad
    , box
    , ibox
    , pushpop
    , place
    , straight
    , turnright
    , turnleft
    , turnaround
    , rightsector
    , innerrightsector
    , innerleftsector
    , popsector
    , leftsector
    , step
    , draw
    , thing
    , mid
    , upper
    , lower
    , xoff
    , yoff
    , ceil
    , floorflat
    , linetype
    , sectortype
    , setthing
    , mapname

    , runWadL

    , htf_thisModulesTests
) where

import Test.Framework hiding (wrap)
import Control.Monad.State.Lazy
import Control.Monad

import Liquorice
import qualified Liquorice.Pure as P

------------------------------------------------------------------------------
-- monadic versions

-- wraps a function above and takes a return value to propagate
wrapR :: a -> (Context -> Context) -> State Context a
wrapR r fn = do
    old <- get
    put (fn old)
    return r

-- return-less version of above
wrap = wrapR ()

-- | Evaluate the supplied State Context to produce a pure Context. In other words,
-- run the supplied Liquorice DSL program and calculate the resulting structure.
runWadL x = snd $ runState x start

-- wrapped pure functions with icky names
turnright         = wrap P.turnright
turnleft          = wrap P.turnleft
turnaround        = wrap P.turnaround
step x y          = wrap $ P.step x y
draw x y          = wrap $ P.draw x y
rightsector f c l = wrap $ P.rightsector f c l
innerrightsector f c l = wrap $ P.innerrightsector f c l
innerleftsector f c l = wrap $ P.innerleftsector f c l
popsector         = wrap $ P.popsector
leftsector  f c l = wrap $ P.leftsector f c l
thing             = wrap P.thing
mid s             = wrap $ P.mid s
upper s           = wrap $ P.upper s
lower s           = wrap $ P.lower s
xoff x            = wrap $ P.xoff x
yoff y            = wrap $ P.yoff y
floorflat s       = wrap $ P.floorflat s
ceil s            = wrap $ P.ceil s
linetype ty ta = wrap $ P.linetype ty ta
sectortype ty ta = wrap $ P.sectortype ty ta
setthing s        = wrap $ P.setthing s
mapname s         = wrap $ P.mapname s

straight n        = draw n 0

blah = runWadL $ do
  straight 64
  turnright
  straight 64
  turnright
  straight 64
  turnright
  straight 64
  turnright
  rightsector 0 128 160

-- | Perform the supplied action twice.
twice = replicateM_ 2

-- | Perform the supplied action three times.
triple x = replicateM_ 3 x

-- | Perform the supplied action four times.
quad x = replicateM_ 4 x

-- | Draw a primitive rectangular box, X and Y in size, and give the resulting
-- sector's properties as `f` floor height, `c` ceiling height and `l` light
-- level.
box x y f c l = do
  twice $ do
    straight x
    turnright
    straight y
    turnright
  rightsector f c l

-- | Draw a primitive rectangular box, similar to `box`, but as an inner sector
-- parented by the last-drawn Sector.
ibox h w f c l = wrap $ P.ibox h w f c l

-- | Perform the actions `x` and then return the pen `location` to the value
-- it had prior to `x`.
pushpop :: State Context () -> State Context ()
pushpop x = do
    old <- get
    x
    new <- get
    put new { location = location old }

-- | Perform the action `stuff at an offset of (`x`,`y`) from the current
-- `location` move the pen back that relative amount afterwards.
place x y stuff = do
    step x y
    r <- stuff
    step (-1 * x) (-1 * y)
    return r
-- XXX this should probably be "and return the pen to that location afterwards."

test_box_orientation = assertEqual (orientation a) (orientation b) where
    a = start
    b = runWadL $ box 64 64 0 0 0

test_box_pos = assertEqual (location a) (location b) where
    a = start
    b = runWadL $ box 64 64 0 0 0

nicerBlah = runWadL $ do
    box 64 64 0 128 160

test_equiv1 = assertEqual blah nicerBlah

blah2 = runWadL (straight 64)
test_lines_generated = assertEqual 1 (length (linedefs blah2))

blah3 = runWadL $ do
    straight 64
    rightsector 0 0 0
test_lines_consumed = assertEqual 0 (length (linedefs blah3))

main = htfMain htf_thisModulesTests
