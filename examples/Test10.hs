{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test10 where

import Data.Typeable
import ExplainInstance

class Foo a where
   foo :: a

instance Foo a where
   foo = error "base"

instance Foo Int where
   foo = 5

$(explainInstanceError [t| Foo Int |])
