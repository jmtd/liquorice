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

wrapPureFunctions = mapM mkWrap pureFns

mkWrap fn = do
    let name = mkName (nameBase fn)

    info    <- reify fn
    let ty   = (\(VarI _ t _ ) -> t) info
    wargs   <- getWrapArgs ty
    args    <- mapM (\_ -> newName "a") wargs

    rhs     <- [| modify $(mkFnApp (varE fn) (zip wargs args)) :: State Context () |]

    return   $ FunD name [ Clause (map VarP args) (NormalB rhs) [] ]


-- convert a list of expressions to function application e.g.
-- mkFnApp f [a,b,c] => ((f a) b) c => f a b c
-- wrap higher-order functions to make them State Context ()
mkFnApp fn zargs = foldl zomg fn zargs
    where zomg e (isHoF, f) = appE e $ if   isHoF
                                       then [| snd . runState $(varE f) |]
                                       else varE f

-- Walk over a Type, return a list of its arguments
-- True: arg needs wrapping (it's a higher order function)
-- False: arg does not need wrapping
-- we throw away the last argument (the input Context)
getWrapArgs :: Type -> Q [Bool]
getWrapArgs (ForallT _ _ remainder) = getWrapArgs remainder
getWrapArgs t = do
    ty <- [t| Context -> Context |]
    return . reverse . tail $ getWrapArgs' ty [] t
    where
        getWrapArgs' ty ns (AppT (AppT ArrowT arg) remainder) =
            getWrapArgs' ty ((arg == ty):ns) remainder
        getWrapArgs' ty ns _ = ns
