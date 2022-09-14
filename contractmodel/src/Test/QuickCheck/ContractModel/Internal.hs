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
        , _balanceChanges :: Map (AddressInEra Era) SymValue
        , _minted         :: SymValue
        , _symTokens      :: Set SymToken
        , _assertions     :: [(String, Bool)]
        , _assertionsOk   :: Bool
        , _contractState  :: state
        }
  deriving (Show)
