{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Test4 where

{- FIXME: Currently broken test of constraint kinds

import ExplainInstance

type OldNum a = (Eq a, Show a, Num a)

class Foo a where
class Bar a where
instance Foo Int
instance OldNum a => Bar (Maybe a) where

-- Looks like this isn't fixed / didn't make it in:
-- https://ghc.haskell.org/trac/ghc/ticket/7021

$(explainInstance [t| Bar (Maybe Int) |])

-}
