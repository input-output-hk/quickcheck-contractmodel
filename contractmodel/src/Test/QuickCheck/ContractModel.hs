module Test.QuickCheck.ContractModel
  ( -- * The safe interface to `SymToken`
    --
    -- NOTE: we don't export the internals here because
    -- it's important that you can't tell the difference
    -- between differnelty numbered SymTokens as these are
    -- not guaranteed to be stable.
    SymToken
  , SymValue
  , symIsZero
  , symLeq
  , toValue
  , toSymVal
  , inv
  , TokenLike(..)
    -- * The safe interface to `Spec`
    --
    -- NOTE: we don't export internals here because we
    -- really don't want people seeing or changing the
    -- sensitive parts of the model.
  , Spec
  ) where

import Test.QuickCheck.ContractModel.Internal.Spec
import Test.QuickCheck.ContractModel.Internal.Symbolics
