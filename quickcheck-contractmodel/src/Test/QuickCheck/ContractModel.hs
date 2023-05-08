module Test.QuickCheck.ContractModel
  ( ContractModel(..)
  , RunModel(..)
  , IsRunnable(..)
  , DefaultRealized
  , RunMonad(..)
  , Actions
  , Act(..)
  , pattern Actions
  , pattern ContractAction
  , pattern WaitUntil
  , stateAfter
  , runContractModel
  , liftRunMonad
  , contractState
  , registerSymbolic
  , registerToken
  , registerTxOut
  , registerTxIn
    -- * Chain index
  , HasChainIndex(..)
  , ChainIndex(..)
  , ChainState(..)
  , TxInState(..)
  , Era
    -- * The safe interface to `Symbolic`
    --
    -- NOTE: we don't export the internals here because
    -- it's important that you can't tell the difference
    -- between differently numbered Symbolics as these numbers
    -- are not guaranteed to be stable.
  , Symbolic
  , SymToken
  , SymTxOut
  , SymTxIn
  , SymValue
  , symIsZero
  , symLeq
  , toValue
  , toSymVal
  , inv
  , SymValueLike(..)
  , TokenLike(..)
  , HasSymbolics(..)
  , HasSymbolicRep
    -- * Properties
  , BalanceChangeOptions(..)
  , assertBalanceChangesMatch
  , signerPaysFees
  , asserts
    -- * Dynamic logic
  , module DL
    -- * The safe interface to `Spec`
    --
    -- NOTE: we don't export internals here because we
    -- really don't want people seeing or changing the
    -- sensitive parts of the model.
  , ModelState
  , Spec(..)
  , GetModelState(..)
  , runSpec
  , currentSlot
  , balanceChanges
  , balanceChange
  , minted
  , lockedValue
  , getContractState
  , askModelState
  , askContractState
  , viewModelState
  , viewContractState
  , createSymbolic
  , createToken
  , createTxOut
  , createTxIn
  , mint
  , burn
  , deposit
  , withdraw
  , transfer
  , waitUntil
  , wait
  , assertSpec
  , coerceSpec
    -- * Internals
  , fromStateModelActions
  ) where

import Test.QuickCheck.ContractModel.Internal
import Test.QuickCheck.ContractModel.Internal.ChainIndex
import Test.QuickCheck.ContractModel.Internal.Common
import Test.QuickCheck.ContractModel.Internal.Model
import Test.QuickCheck.ContractModel.Internal.Spec
import Test.QuickCheck.ContractModel.Internal.Symbolics

import Test.QuickCheck.ContractModel.DL as DL
