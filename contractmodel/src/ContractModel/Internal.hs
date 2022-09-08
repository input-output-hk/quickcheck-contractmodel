module ContractModel.Internal where

import Test.QuickCheck
import Test.QuickCheck.StateModel

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
