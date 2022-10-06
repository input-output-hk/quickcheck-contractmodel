module Test.QuickCheck.ContractModel.Internal.ChainIndex where

import Data.Ord
import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Cardano.Api
import Cardano.Api.Shelley

import Test.QuickCheck.ContractModel.Internal.Common
import Test.QuickCheck.ContractModel.Internal.Utils

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

allMinUTxO :: ChainIndex
           -> ProtocolParameters
           -> [Lovelace]
allMinUTxO ci params =
  [ selectLovelace v
  | TxInState{..} <- transactions ci
  , txOut <- getTxOuts tx
  , Right v <- [calculateMinimumUTxO era txOut params]
  ]

type FeeCalculation = TxInState -> Map (AddressInEra Era) Value

signerPaysFees :: FeeCalculation
signerPaysFees TxInState{..} = _

-- TODO: what about failing transactions?
getBalanceChangesWithoutFees :: ChainIndex
                             -> FeeCalculation
                             -> Map (AddressInEra Era) Value
getBalanceChangesWithoutFees ChainIndex{..} computeFees =
  foldr (Map.unionWith (<>)) mempty $  map txBalanceChanges transactions
                                    ++ map computeFees transactions

txBalanceChanges :: TxInState
                 -> Map (AddressInEra Era) Value
txBalanceChanges (TxInState tx ChainState{..}) =
  Map.unionsWith (<>) $ [ Map.singleton a (txOutValueToValue v)
                        | TxOut a v _ _ <- getTxOuts tx
                        ] ++
                        [ Map.singleton a (negateValue $ txOutValueToValue v)
                        | TxOut a v _ _ <- getTxInputs tx utxo
                        ]
