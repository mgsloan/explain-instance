{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- Test of datatype contexts
module Test8 where

import ExplainInstance

data Num a => A a = A a

instance Functor A where
    fmap = undefined

explainInstance [t| Functor A |]
