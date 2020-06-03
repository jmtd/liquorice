{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-|
Module      : Liquorice.Monad.Test
Description : Tests for Liquorice.Monad.
Copyright   : © Jonathan Dowland, 2020
License     : GPL-3
Maintainer  : jon+hackage@dow.land
Stability   : experimental
Portability : POSIX

Tests for Liquorice.Monad.
-}
module Liquorice.Monad.Test (htf_thisModulesTests) where

import Liquorice
import Liquorice.Monad
import Test.Framework hiding (wrap)

blah = runWadL $ do
  quad $ do
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
