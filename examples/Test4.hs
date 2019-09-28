{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Note: this is currently expected to not work.  Would need special
-- logic for handling constraint kind type synonyms.
module Test4 where

import ExplainInstance

type OldNum a = (Eq a, Show a, Num a)

class Foo a where
class Bar a where
instance Foo Int
instance OldNum a => Bar (Maybe a) where

$(explainInstance [t| Bar (Maybe Int) |])
