-- Auto-generated file.
-- Do not alter.

{-# LANGUAGE NoImplicitPrelude, DataKinds, BinaryLiterals #-}
module Configuration.Generated.Materialize where

import Clash.Prelude

materialize iStream dStream = ((iStream ++ repeat 0) :: Vec (32) (BitVector 32), (dStream ++ repeat 0) :: Vec (64) (BitVector 32))