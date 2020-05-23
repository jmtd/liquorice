{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Liquorice.Monad.TH
Description : Internal module for Liquorice.Monad
Copyright   : © Jonathan Dowland, 2020
License     : GPL-3
Maintainer  : jon+hackage@dow.land
Stability   : experimental
Portability : POSIX

These routines are internal for Liquorice but need to be defined within
a separate module from Liquorice.Monad due to Template Haskell staging
constraints.
-}
module Liquorice.Monad.TH where

import Control.Monad.State.Lazy
import Language.Haskell.TH hiding (location)
import Test.Framework hiding (wrap)

import Liquorice
import Liquorice.Pure

wrapPureFunctions = liftM concat (mapM mkWrap pureFns)

mkWrap fn = do
    info    <- reify fn
    let ty   = (\(VarI _ t _ ) -> t) info
    let n    = arity ty - 1
    let name = mkName (nameBase fn)
    args    <- replicateM n (newName "arg")
    rhs     <- [| modify $(mkFnApp (varE fn) args) :: State Context () |]
    return [FunD name [ Clause (map VarP args) (NormalB rhs) [] ]]

arity :: Type -> Int
arity = arity' 0 where
    arity' n (AppT (AppT ArrowT _) remainder) = arity' (n+1) remainder
    arity' n (ForallT _ _ remainder) = arity' n remainder
    arity' n _ = n

-- convert a list of expressions to function application e.g.
-- mkFnApp f [a,b,c] => ((f a) b) c => f a b c
mkFnApp = foldl (\e -> appE e . varE)
