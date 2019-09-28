{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}

module Test2 where

import ExplainInstance

class Foo a where
class Bar a where
instance Foo Int
instance Foo a => Bar (Maybe a) where

$(explainInstance [t| Bar (Maybe Int) |])
