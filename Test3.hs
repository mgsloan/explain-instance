{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Test3 where

import ExplainInstance
import Text.Printf

$(explainInstance [t| PrintfType (Int -> Int -> String) |])
