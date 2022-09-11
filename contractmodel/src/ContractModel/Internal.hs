module ContractModel.Internal where

import Test.QuickCheck
import Test.QuickCheck.StateModel

import Cardano.Api

type Era = BabbageEra

-- TODO:
-- * Figure out how to talk to node?!
-- * Import old ContractModel class
-- * Figure out where we need to split the contract model
--   class to allow multiple backends (probably along the same
--   lines as StateModel) (might be worth-while to do this
--   in a branch of plutus-apps)
-- * Figure out where to add general "chain indexing" stuff to deal
--   with multiple backends having the same definitions of properties.
--   Easiest way is if there is a way to get a [(Tx Era, UTxO)] out
--   of the backend:
--
--   runTests :: RunContractModel state m => m (UTxO {- current chain state -}, [(Tx Era, UTxO)] {- all txs that
--   happened and in what chain state they happened -})
--
--   Possible that you also want to get out failed txs here or something along
--   those lines (to do properties like exception whitelists).
--   To be decided in the future... (not prio 1)
--
-- * Build old useful properties from this general interface
--
-- * Instantiate interface with node, emulator, iosim, etc. etc. etc....

data ChainIndex = ChainIndex
  { before       :: UTxO Era
  , after        :: UTxO Era
  -- TODO: this probably also needs to know what slot we are in (i.e. we need to know all the
  -- state that determines the result
  , transactions :: [(Tx Era, UTxO Era)]
  }

instance Semigroup ChainIndex where
  ci <> ci' = ChainIndex { before = before ci
                         , after  = after ci'
                         , transactions = transactions ci ++ transactions ci'
                         }

class ContractTestable m where
  withChainIndex :: m a -> m (a, ChainIndex)
