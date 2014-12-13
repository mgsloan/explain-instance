{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}

module Test where

import Data.Proxy
import ExplainInstances

$(instanceResolvers [''Ord])

main = print (resolveOrd (Proxy :: Proxy [Maybe Int]))
