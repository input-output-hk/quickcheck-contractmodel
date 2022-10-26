module Test.QuickCheck.ContractModel.Internal.ChainIndex where

import Data.Ord
import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Cardano.Api
import Cardano.Api.Shelley
import Cardano.Ledger.Shelley.TxBody (WitVKey (..))
import Cardano.Ledger.Keys (hashKey, coerceKeyRole)

import Test.QuickCheck.ContractModel.Internal.Common
import Test.QuickCheck.ContractModel.Internal.Utils

data ChainState = ChainState
  { slot :: SlotNo
  , utxo :: UTxO Era
  }

data TxInState = TxInState
  { tx         :: Tx Era
  , chainState :: ChainState
  , accepted   :: Bool
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

allMinAda :: ChainIndex
          -> ProtocolParameters
          -> [Lovelace]
allMinAda ci params =
  [ selectLovelace v
  | TxInState{..} <- transactions ci
  , txOut <- getTxOuts tx
  , Right v <- [calculateMinimumUTxO era txOut params]
  , accepted
  ]

type FeeCalculation = TxInState -> Map (AddressInEra Era) Value

signerPaysFees :: FeeCalculation
signerPaysFees TxInState{tx = tx, accepted = accepted}
  | not accepted = error "TODO: signerPaysFees rejected tx"
  | Tx (TxBody (txFee -> TxFeeExplicit _ lov)) [wit] <- tx = Map.singleton (shelleyAddressInEra $ mkAddrFromWitness wit) (lovelaceToValue lov)
  | otherwise = mempty

-- TODO: is this really safe?? also - why is this so complicated??
mkAddrFromWitness :: KeyWitness Era -> Address ShelleyAddr
mkAddrFromWitness wit = makeShelleyAddress Mainnet
                                           (keyHashObj wit)
                                           NoStakeAddress
  where keyHashObj :: KeyWitness Era -> PaymentCredential
        keyHashObj (ShelleyKeyWitness _ (WitVKey wit _)) =
            PaymentCredentialByKey
          . PaymentKeyHash
          . hashKey
          . coerceKeyRole   -- TODO: is this really safe?!?!?!
          $ wit
        keyHashObj ShelleyBootstrapWitness{} = error "keyHashObj: ShelleyBootstrapWitness{}"

-- TODO: what about failing transactions?
getBalanceChangesDiscountingFees :: ChainIndex
                                 -> FeeCalculation
                                 -> Map (AddressInEra Era) Value
getBalanceChangesDiscountingFees ChainIndex{..} computeFees =
  foldr (Map.unionWith (<>)) mempty $  map txBalanceChanges transactions
                                    ++ map computeFees transactions

txBalanceChanges :: TxInState
                 -> Map (AddressInEra Era) Value
txBalanceChanges (TxInState tx ChainState{..} accepted)
  | accepted = Map.unionsWith (<>) $ [ Map.singleton a (txOutValueToValue v)
                                     | TxOut a v _ _ <- getTxOuts tx
                                     ] ++
                                     [ Map.singleton a (negateValue $ txOutValueToValue v)
                                     | TxOut a v _ _ <- getTxInputs tx utxo
                                     ]
  | otherwise = error "TODO txBalanceChanges when removing collateral"
