{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main where

import Test.Framework

import {-@ HTF_TESTS @-} Liquorice
import {-@ HTF_TESTS @-} Liquorice.Pure
import {-@ HTF_TESTS @-} Liquorice.Monad as M
import {-@ HTF_TESTS @-} Liquorice.Render
import {-@ HTF_TESTS @-} Liquorice.Line
import {-@ HTF_TESTS @-} Liquorice.Wad

test_example9 = let pre  = addLine (Line (0,0) (0,128) "" "" "" 0 0 0 0) start
                    post = addLine (Line (0,64) (0,192) "" "" "" 0 0 0 0) pre
                in  assertEqual 3 (length (linedefs post))

main = htfMain htf_importedTests
