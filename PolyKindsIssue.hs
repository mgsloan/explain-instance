{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}

{- Run with -ddump-splices to get

PolyKindsIssue.hs:1:1: Splicing declarations
    do { ClassI clazz insts <- fmap
                                 (everywhere (id `extT` (mkName . nameBase)))
                               $ reify ''C.TestCoercion;
         return (clazz : insts) }
  ======>
    PolyKindsIssue.hs:(17,1)-(18,25)
    class TestCoercion (f :: k -> *) where
      testCoercion ::
        forall (f :: k -> *). TestCoercion k f =>
        forall (a :: k) (b :: k). f a -> f b -> Maybe (Coercion a b)
    instance TestCoercion k (Coercion a)
    instance TestCoercion k ((:~:) a)

PolyKindsIssue.hs:36:1:
    ‘TestCoercion’ is applied to too many type arguments
    In the type ‘forall (f :: k -> *). TestCoercion k f =>
                 forall (a :: k) (b :: k). f a -> f b -> Maybe (Coercion a b)’
    In the class declaration for ‘TestCoercion’
Failed, modules loaded: none.

-}
module PolyKindsIssue where

import           Data.Generics
import           Data.Type.Coercion (Coercion)
import qualified Data.Type.Coercion as C
import           Language.Haskell.TH

do ClassI clazz insts <- fmap (everywhere (id `extT` (mkName . nameBase))) $ reify ''C.TestCoercion
   return (clazz : insts)
