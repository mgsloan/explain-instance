{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Test9 where

import ExplainInstance

class Size a where
    size :: a -> Int

instance Size Int where
    size = id

instance Size a => Size (Maybe a) where
    size = maybe 0 size

instance (Size a, Size b) => Size (a, b) where
    size (a, b) = size a + size b

$(explainInstance [t| Size (Maybe Int, Int) |])
