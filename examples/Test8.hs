{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- Test of datatype contexts
module Test8 where

{- FIXME: Broken

import ExplainInstance

data Num a => A a = A a

instance Functor A where
    fmap = undefined

explainInstance [t| Functor A |]

-}
