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

import Data.Conduit

-- Imports needed to make instances visible to GHC reification
import Data.Functor.Const
import Data.Functor.Identity
import Foreign.C.Types
import GHC.Int
import GHC.Word
import Control.Monad.IO.Class
import Control.Monad.Reader

$(explainInstance [t| MonadIO (ConduitT String Int (ReaderT Int IO)) |])
