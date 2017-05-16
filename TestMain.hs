{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main where

import Test.Framework

import {-@ HTF_TESTS @-} Liquorice
import {-@ HTF_TESTS @-} Liquorice.Pure
import {-@ HTF_TESTS @-} Liquorice.Monad as M
import {-@ HTF_TESTS @-} Render
import {-@ HTF_TESTS @-} Line
import {-@ HTF_TESTS @-} Wad

main = htfMain htf_importedTests
