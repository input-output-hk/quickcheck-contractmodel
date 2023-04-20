
module Test.QuickCheck.ContractModel.ThreatModel.Cardano.Api where

import Cardano.Api
import Cardano.Api.Byron
import Cardano.Api.Shelley
import Cardano.Ledger.Alonzo.Tx qualified as Ledger (Data, hashData, indexOf)
import Cardano.Ledger.Alonzo.TxWitness qualified as Ledger
import Cardano.Ledger.Babbage.TxBody qualified as Ledger
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Keys (coerceKeyRole, hashKey)
import Cardano.Ledger.Shelley.TxBody (Wdrl (..), WitVKey (..))
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval (..))
import Cardano.Ledger.TxIn (txid)
import Cardano.Slotting.Slot (EpochSize (EpochSize))
import Cardano.Slotting.Time (SlotLength, mkSlotLength)
import Ouroboros.Consensus.Cardano.Block (CardanoEras)
import Ouroboros.Consensus.HardFork.History
import Ouroboros.Consensus.Util.Counting (NonEmpty (NonEmptyOne))
import PlutusTx (ToData, toData)
import Cardano.Ledger.Alonzo.Scripts qualified as Ledger

import Data.Either
import Data.Map qualified as Map
import Data.Maybe.Strict
import Data.Sequence.Strict qualified as Seq
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word

import Test.QuickCheck.ContractModel.Internal.Common

addressOfTxOut :: TxOut ctx Era -> AddressAny
addressOfTxOut (TxOut (AddressInEra ShelleyAddressInEra{}  addr) _ _ _) = AddressShelley addr
addressOfTxOut (TxOut (AddressInEra ByronAddressInAnyEra{} addr) _ _ _) = AddressByron   addr

valueOfTxOut :: TxOut ctx Era -> Value
valueOfTxOut (TxOut _ (TxOutAdaOnly _ v) _ _) = lovelaceToValue v
valueOfTxOut (TxOut _ (TxOutValue _ v) _ _)   = v

-- | Get the datum from a transaction output.
datumOfTxOut :: TxOut ctx Era -> TxOutDatum ctx Era
datumOfTxOut (TxOut _ _ datum _) = datum

redeemerOfTxIn :: Tx Era -> TxIn -> Maybe ScriptData
redeemerOfTxIn tx txIn = redeemer
  where
    Tx (ShelleyTxBody _ Ledger.TxBody{Ledger.inputs=inputs} _ scriptData _ _) _ = tx

    redeemer = case scriptData of
      TxBodyNoScriptData -> Nothing
      TxBodyScriptData _ _ (Ledger.Redeemers rdmrs) ->
        fromAlonzoData . fst <$> Map.lookup (Ledger.RdmrPtr Ledger.Spend idx) rdmrs

    idx = case Ledger.indexOf (toShelleyTxIn txIn) inputs of
      SJust idx -> idx
      _         -> error "The impossible happened!"

paymentCredentialToAddressAny :: PaymentCredential -> AddressAny
paymentCredentialToAddressAny t =
  AddressShelley $ makeShelleyAddress (Testnet $ NetworkMagic 1) t NoStakeAddress

-- | Construct a script address.
scriptAddressAny :: ScriptHash -> AddressAny
scriptAddressAny = paymentCredentialToAddressAny . PaymentCredentialByScript

-- | Construct a public key address.
keyAddressAny :: Hash PaymentKey -> AddressAny
keyAddressAny = paymentCredentialToAddressAny . PaymentCredentialByKey

-- | Check if an address is a public key address.
isKeyAddressAny :: AddressAny -> Bool
isKeyAddressAny = isKeyAddress . anyAddressInShelleyBasedEra @Era

recomputeScriptData :: Maybe Word64 -- Index to remove
                    -> (Word64 -> Word64)
                    -> TxBodyScriptData Era
                    -> TxBodyScriptData Era
recomputeScriptData _ _ TxBodyNoScriptData = TxBodyNoScriptData
recomputeScriptData i f (TxBodyScriptData era dats (Ledger.Redeemers rdmrs)) =
  TxBodyScriptData era dats
    (Ledger.Redeemers $ Map.mapKeys updatePtr $ Map.filterWithKey idxFilter rdmrs)
  where updatePtr (Ledger.RdmrPtr tag idx) = Ledger.RdmrPtr tag (f idx)
        idxFilter (Ledger.RdmrPtr _ idx) _ = Just idx /= i

emptyTxBodyScriptData :: TxBodyScriptData Era
emptyTxBodyScriptData = TxBodyScriptData ScriptDataInBabbageEra (Ledger.TxDats mempty) (Ledger.Redeemers mempty)

addScriptData :: Word64
              -> Ledger.Data (ShelleyLedgerEra Era)
              -> (Ledger.Data (ShelleyLedgerEra Era), Ledger.ExUnits)
              -> TxBodyScriptData Era
              -> TxBodyScriptData Era
addScriptData ix dat rdmr TxBodyNoScriptData = addScriptData ix dat rdmr emptyTxBodyScriptData
addScriptData ix dat rdmr (TxBodyScriptData era (Ledger.TxDats dats) (Ledger.Redeemers rdmrs)) =
  TxBodyScriptData era (Ledger.TxDats $ Map.insert (Ledger.hashData dat) dat dats)
                       (Ledger.Redeemers $ Map.insert (Ledger.RdmrPtr Ledger.Spend ix) rdmr rdmrs)

addDatum :: Ledger.Data (ShelleyLedgerEra Era)
         -> TxBodyScriptData Era
         -> TxBodyScriptData Era
addDatum dat TxBodyNoScriptData = addDatum dat emptyTxBodyScriptData
addDatum dat (TxBodyScriptData era (Ledger.TxDats dats) rdmrs) =
  TxBodyScriptData era (Ledger.TxDats $ Map.insert (Ledger.hashData dat) dat dats)
                       rdmrs

toCtxUTxODatum :: TxOutDatum CtxTx Era -> TxOutDatum CtxUTxO Era
toCtxUTxODatum d = case d of
  TxOutDatumNone        -> TxOutDatumNone
  TxOutDatumHash s h    -> TxOutDatumHash s h
  TxOutDatumInTx s d    -> TxOutDatumHash s (hashScriptData d)
  TxOutDatumInline s sd -> TxOutDatumInline s sd

-- | Convert ScriptData to a `Test.QuickCheck.ContractModel.ThreatModel.Datum`.
txOutDatum :: ScriptData -> TxOutDatum CtxTx Era
txOutDatum d = TxOutDatumInTx ScriptDataInBabbageEra d

-- | Convert a Haskell value to ScriptData for use as a
-- `Test.QuickCheck.ContractModel.ThreatModel.Redeemer` or convert to a
-- `Test.QuickCheck.ContractModel.ThreatModel.Datum` with `txOutDatum`.
toScriptData :: ToData a => a -> ScriptData
toScriptData = fromPlutusData . toData

-- | Used for new inputs.
dummyTxId :: TxId
dummyTxId =
  fromShelleyTxId
  $ txid @LedgerEra
  $ Ledger.TxBody @LedgerEra
      mempty
      mempty
      mempty
      Seq.empty
      SNothing
      SNothing
      Seq.empty
      (Wdrl mempty)
      mempty
      (ValidityInterval SNothing SNothing)
      SNothing
      mempty
      mempty
      SNothing
      SNothing
      SNothing

makeTxOut :: AddressAny -> Value -> TxOutDatum CtxTx Era -> ReferenceScript Era -> TxOut CtxUTxO Era
makeTxOut addr value datum refScript =
  toCtxUTxOTxOut $ TxOut (anyAddressInShelleyBasedEra addr)
                         (TxOutValue MultiAssetInBabbageEra value)
                         datum
                         refScript

txSigners :: Tx Era -> [Hash PaymentKey]
txSigners (Tx _ wits) = [ toHash wit | ShelleyKeyWitness _ (WitVKey wit _) <- wits ]
  where
    toHash = PaymentKeyHash
           . hashKey
           . coerceKeyRole

txInputs :: Tx Era -> [TxIn]
txInputs (Tx (TxBody body) _) = map fst $ txIns body

txOutputs :: Tx Era -> [TxOut CtxTx Era]
txOutputs (Tx (TxBody body) _) = txOuts body

-- | Check if a value is less or equal than another value.
leqValue :: Value -> Value -> Bool
leqValue v v' = all ((<= 0) . snd) (valueToList $ v <> negateValue v')

-- | Keep only the Ada part of a value.
projectAda :: Value -> Value
projectAda = lovelaceToValue . selectLovelace

-- TODO: transactions can fail for different reasons. Sometimes they fail with
-- a "translation error". Translation errors should probably be treated as test
-- failures not as validation failing - it's after all not validation failing!

-- | The result of validating a transaction. In case of failure, it includes a list
--   of reasons.
data ValidityReport = ValidityReport
  { valid  :: Bool
  , errors :: [String]
  } deriving stock (Ord, Eq, Show)

-- NOTE: this function ignores the execution units associated with
-- the scripts in the Tx. That way we don't have to care about computing
-- the right values in the threat model (as this is not our main concern here).
--
-- This also means that if we were to want to deal with execution units in the threat
-- modelling framework we would need to be a bit careful and figure out some abstractions
-- that make it make sense (and check the budgets here).
--
-- Stolen from Hydra
validateTx :: ProtocolParameters -> Tx Era -> UTxO Era -> ValidityReport
validateTx pparams tx utxos = case result of
  Left e -> ValidityReport False [show e]
  Right report -> ValidityReport (all isRight (Map.elems report))
                                 [show e | Left e <- Map.elems report]
  where
    result = evaluateTransactionExecutionUnits
                BabbageEraInCardanoMode
                systemStart
                eraHistory
                pparams
                utxos
                (getTxBody tx)
    eraHistory :: EraHistory CardanoMode
    eraHistory = EraHistory CardanoMode (mkInterpreter summary)

    summary :: Summary (CardanoEras StandardCrypto)
    summary =
      Summary . NonEmptyOne $
        EraSummary
          { eraStart = initBound
          , eraEnd = EraUnbounded
          , eraParams =
              EraParams
                { eraEpochSize = epochSize
                , eraSlotLength = slotLength
                , eraSafeZone = UnsafeIndefiniteSafeZone
                }
          }

    epochSize :: EpochSize
    epochSize = EpochSize 100

    slotLength :: SlotLength
    slotLength = mkSlotLength 1

    systemStart :: SystemStart
    systemStart = SystemStart $ posixSecondsToUTCTime 0

-- | Keep only UTxOs mentioned in the given transaction.
restrictUTxO :: Tx Era -> UTxO Era -> UTxO Era
restrictUTxO (Tx (TxBody TxBodyContent{..}) _) (UTxO utxo) =
  UTxO $ Map.filterWithKey (\ k _ -> k `elem` map fst txIns) utxo

convValidityInterval
  :: (TxValidityLowerBound era, TxValidityUpperBound era)
  -> ValidityInterval
convValidityInterval (lowerBound, upperBound) =
  ValidityInterval
    { invalidBefore = case lowerBound of
                        TxValidityNoLowerBound   -> SNothing
                        TxValidityLowerBound _ s -> SJust s
    , invalidHereafter = case upperBound of
                           TxValidityNoUpperBound _ -> SNothing
                           TxValidityUpperBound _ s -> SJust s
    }

