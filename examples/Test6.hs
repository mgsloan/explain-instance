{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test6 where

import Data.Typeable
import Database.Persist
import Database.Persist.Sql
import ExplainInstance

-- Imports needed to make instances visible to GHC reification
import GHC.Unicode
import Data.Functor.Const
import Data.Functor.Identity
import Data.Proxy

deriving instance Typeable Key
deriving instance Typeable BackendKey

$(explainInstance [t| PersistQuery SqlBackend |])
