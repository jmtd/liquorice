{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main where

import Test.Framework

import {-@ HTF_TESTS @-} Liquorice
import {-@ HTF_TESTS @-} Liquorice.Pure
import {-@ HTF_TESTS @-} Liquorice.Monad as M
import {-@ HTF_TESTS @-} Liquorice.Render
import {-@ HTF_TESTS @-} Liquorice.Line
import {-@ HTF_TESTS @-} Doom.Wad

main = htfMain htf_importedTests
