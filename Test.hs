{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}

-- NOTE: doesn't yet work due to issues with TH and PolyKinds

module Test where

import ExplainInstances

$(explainInstance [t| Ord [Maybe Int] |])
