{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test3 where

import Data.Proxy
import ExplainInstances
import Text.Printf

$(instanceResolvers [''PrintfType])

main = putStrLn $ displayInst $ resolvePrintfType (Proxy :: Proxy (Int -> Int -> String))
