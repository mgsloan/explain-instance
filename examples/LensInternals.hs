{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Typeable
import ExplainInstance

import Control.Lens
import Control.Lens.Internal.Context

-- Imports needed to make instances visible to GHC reification
import Data.Functor.Identity
import Control.Comonad
import Data.Complex
import Data.Ord
import Data.Either
import Data.Vector.Fusion.Util

$(explainInstance [t| Contravariant (PretextT (->) (Const Int) String String) |])
