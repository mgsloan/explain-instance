{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test3 where

import ExplainInstances
import Text.Printf

$(explainInstance [t| PrintfType (Int -> Int -> String) |])
