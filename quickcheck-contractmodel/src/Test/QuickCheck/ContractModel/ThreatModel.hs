{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -Wno-unused-matches -Wno-name-shadowing #-}
-- | The threat modelling framework allows you to write down and test properties of modifications of
--   valid transactions.
--
--   A threat model is represented by a value in the `ThreatModel` monad, and is evaluated in the
--   context of a single valid transaction and the chain state at the point it validated (a
--   `ThreatModelEnv`). Transactions and chain states can most easily be obtained using a
--   `ContractModelResult` from `runContractModel`, but they can in principle come from anywhere.
--
--   As an example, here is a `ThreatModel` that checks that any interaction with @myScript@
--   requires @theToken@ to be present:
--
-- @
--     tokenThreatModel :: 'ThreatModel' ()
--     tokenThreatModel = do
--       'ensureHasInputAt' myScript
--
--       let hasToken out = theToken ``leqValue`` 'valueOf' out
--       i <- 'anyInputSuchThat'  hasToken
--       o <- 'anyOutputSuchThat' hasToken
--
--       'shouldNotValidate' $ 'changeValueOf' i ('valueOf' i <> negateValue theToken)
--                        <> 'changeValueOf' o ('valueOf' o <> negateValue theToken)
-- @
--
--   For a more complex example see "Test.QuickCheck.ThreatModel.DoubleSatisfaction".
module Test.QuickCheck.ContractModel.ThreatModel
  ( assertThreatModel
  , module X
  ) where

import Cardano.Api.Shelley

import Test.QuickCheck.ContractModel.Internal
import Test.QuickCheck.ContractModel.Internal.ChainIndex
import Test.QuickCheck.ContractModel.Internal.Common

import Test.QuickCheck

import Test.QuickCheck.ThreatModel as X hiding (assertThreatModel)
import Test.QuickCheck.ThreatModel qualified as TM

-- | Evaluate a `ThreatModel` on the result of running a t`Test.QuickCheck.ContractModel.ContractModel` test (see
--   `runContractModel`). Checks the threat model on all transactions produced by the test.
assertThreatModel :: ThreatModel a
                  -> LedgerProtocolParameters Era
                  -> ContractModelResult state
                  -> Property
assertThreatModel m params result = TM.assertThreatModel m params envs
  where
    envs = [ (tx txInState, utxo $ chainState txInState)
           | txInState <- transactions $ finalChainIndex result ]
