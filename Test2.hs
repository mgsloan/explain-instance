{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test2 where

import Data.Proxy
import ExplainInstances

class Foo a where
class Bar a where
instance Foo Int
instance Foo a => Bar (Maybe a) where

$(instanceResolvers [''Bar])

main = putStrLn $ displayInst $ resolveBar (Proxy :: Proxy (Maybe Int))
