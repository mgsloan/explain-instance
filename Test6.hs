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

--FIXME: Currently broken

module Test6 where

import Data.Typeable
import Database.Persist
import Database.Persist.Sql
import ExplainInstance

deriving instance Typeable Key
deriving instance Typeable BackendKey

$(explainInstance [t| PersistQuery SqlBackend |])
