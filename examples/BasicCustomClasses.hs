{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}

import ExplainInstance

class Foo a where
class Bar a where
instance Foo Int
instance Foo a => Bar (Maybe a) where

$(explainInstance [t| Bar (Maybe Int) |])
