{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Test7 where

import Data.Typeable
import ExplainInstances

import Control.Lens.Internal.Context
import Control.Lens

deriving instance Typeable Const

$(explainInstance [t|   Contravariant (PretextT (->) (Const Int) String String ) |])
