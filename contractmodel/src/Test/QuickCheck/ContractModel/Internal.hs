module Test.QuickCheck.ContractModel.Internal where

import Test.QuickCheck
import Test.QuickCheck.StateModel
import Test.QuickCheck.ContractModel.Symbolics
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Ord
import Data.List

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

data ChainState = ChainState
  { slot :: SlotNo
  , utxo :: UTxO Era
  }

data TxInState = TxInState
  { tx         :: Tx Era
  , chainState :: ChainState
  }

data ChainIndex = ChainIndex
  { before       :: ChainState
  , after        :: ChainState
  , transactions :: [TxInState]
  }

instance Semigroup ChainIndex where
  ci <> ci' = ChainIndex { before       = minimumBy (comparing slot) [before ci, before ci']
                         , after        = maximumBy (comparing slot) [after ci, after ci']
                         , transactions = sortBy (comparing (slot . chainState))
                                        $ transactions ci ++ transactions ci'
                         }

class ContractTestable m where
  withChainIndex :: m a -> m (a, ChainIndex)

-- | The `ModelState` models the state of the blockchain. It contains,
--
--   * the contract-specific state (`contractState`)
--   * the current slot (`currentSlot`)
--   * the wallet balances (`balances`)
--   * the amount that has been minted (`minted`)
data ModelState state = ModelState
        { _currentSlot    :: SlotNo
        , _balanceChanges :: Map Wallet SymValue
        , _minted         :: SymValue
        , _symTokens      :: Set SymToken
        , _assertions     :: [(String, Bool)]
        , _assertionsOk   :: Bool
        , _contractState  :: state
        }
  deriving (Show)
