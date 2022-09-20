module Test.QuickCheck.ContractModel.Internal.ChainIndex where

import Data.Ord
import Data.List
import Cardano.Api

import Test.QuickCheck.ContractModel.Internal.Common

data ChainState = ChainState
  { slot :: SlotNo
  , utxo :: UTxO Era
  }

data TxInState = TxInState
  { tx         :: Tx Era
  , chainState :: ChainState
  }

-- TODO: this before-after stuff is a bit suspect!
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

class HasChainIndex m where
  getChainIndex :: m ChainIndex
