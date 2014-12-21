{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test5 where

import Data.Monoid
import Data.Typeable
import ExplainInstance
import Text.Printf

data A = A deriving (Typeable)

$(explainInstanceError [t| PrintfType (A -> Int -> Maybe String) |])
