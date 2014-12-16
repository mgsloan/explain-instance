{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Test5 where

import Data.Monoid
import Data.Typeable
import ExplainInstances
import Text.Printf

data A = A deriving (Typeable)

$(explainInstanceError [t| PrintfType (A -> Int -> Maybe String) |])
