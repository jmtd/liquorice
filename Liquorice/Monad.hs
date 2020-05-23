{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE TemplateHaskell #-}

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
    , withXoff

    , runWadL

    , htf_thisModulesTests
) where

import Test.Framework hiding (wrap)
import Control.Monad.State.Lazy
import Control.Monad
import Language.Haskell.TH hiding (location)

import Liquorice
import qualified Liquorice.Pure as P
import Liquorice.Monad.TH

-- injects monadic-wrapped versions of the functions from Liquorice.Pure
wrapPureFunctions

-- | Evaluate the supplied State Context to produce a pure Context. In other words,
-- run the supplied Liquorice DSL program and calculate the resulting structure.
runWadL x = snd $ runState x start

-------------------------------------------------------------------
-- the following functions are re-implemented rather than wrapped,
-- as wrapping them would require translating their arguments of
-- type (Context -> Context) to (State Context ()) and sequencing
-- them

-- | Perform the supplied action twice.
twice x = replicateM_ 2 x

-- | Perform the supplied action three times.
triple x = replicateM_ 3 x

-- | Perform the supplied action four times.
quad x = replicateM_ 4 x

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

-- | Perform the supplied actions with `paletteXoff` set to the supplied value,
-- then reset `paletteXoff`.
withXoff :: Int -> State Context () -> State Context ()
withXoff x c = do
    old <- get
    xoff x
    c
    xoff (paletteXoff old)

------------------------------------------------------------------------------
-- tests

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
