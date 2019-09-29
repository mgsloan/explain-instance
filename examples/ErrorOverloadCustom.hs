{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Typeable
import ExplainInstance

class Foo a
instance {-# OVERLAPPABLE #-} Foo a
instance {-# OVERLAPPING #-} Foo Int

$(explainInstanceError [t| Foo Int |])
