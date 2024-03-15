
module Test.QuickCheck.ThreatModel.Cardano.Api where

import Cardano.Api
import Cardano.Api.Byron
import Cardano.Api.Shelley
import Cardano.Ledger.Api.Tx.Body qualified as Ledger
import Cardano.Ledger.Alonzo.TxWits qualified as Ledger
import Cardano.Ledger.Alonzo.TxBody qualified as Ledger
import Cardano.Ledger.Alonzo.Scripts qualified as Ledger
import Cardano.Ledger.Conway.Scripts qualified as Ledger
import Cardano.Ledger.Conway.TxBody qualified as Ledger
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Keys (WitVKey (..), coerceKeyRole, hashKey)
import Cardano.Ledger.Allegra.Scripts (ValidityInterval (..))
import Cardano.Slotting.Slot (EpochSize (EpochSize))
import Cardano.Slotting.Time (SlotLength, mkSlotLength)
import Ouroboros.Consensus.Cardano.Block (CardanoEras)
import Ouroboros.Consensus.HardFork.History
import Data.SOP.NonEmpty (NonEmpty (NonEmptyOne))
import PlutusTx (ToData, toData)

import Data.Either
import Data.Map qualified as Map
import Data.Maybe.Strict
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word

type Era = ConwayEra
type LedgerEra = ShelleyLedgerEra Era

addressOfTxOut :: TxOut ctx Era -> AddressAny
addressOfTxOut (TxOut (AddressInEra ShelleyAddressInEra{}  addr) _ _ _) = AddressShelley addr
addressOfTxOut (TxOut (AddressInEra ByronAddressInAnyEra{} addr) _ _ _) = AddressByron   addr

valueOfTxOut :: TxOut ctx Era -> Value
valueOfTxOut (TxOut _ v _ _) = txOutValueToValue v

-- | Get the datum from a transaction output.
datumOfTxOut :: TxOut ctx Era -> TxOutDatum ctx Era
datumOfTxOut (TxOut _ _ datum _) = datum

referenceScriptOfTxOut :: TxOut ctx Era -> ReferenceScript Era
referenceScriptOfTxOut (TxOut _ _ _ rscript) = rscript

redeemerOfTxIn :: Tx Era -> TxIn -> Maybe ScriptData
redeemerOfTxIn tx txIn = redeemer
  where
    Tx (ShelleyTxBody _ Ledger.ConwayTxBody{Ledger.ctbSpendInputs=inputs} _ scriptData _ _) _ = tx

    redeemer = case scriptData of
      TxBodyNoScriptData -> Nothing
      TxBodyScriptData _ _ (Ledger.Redeemers rdmrs) ->
        getScriptData . fromAlonzoData . fst <$> Map.lookup (Ledger.ConwaySpending idx) rdmrs

    idx = case Ledger.indexOf (Ledger.AsItem (toShelleyTxIn txIn)) inputs of
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
isKeyAddressAny = isKeyAddress . anyAddressInShelleyBasedEra (shelleyBasedEra @Era)

recomputeScriptData :: Maybe Word32 -- Index to remove
                    -> (Word32 -> Word32)
                    -> TxBodyScriptData Era
                    -> TxBodyScriptData Era
recomputeScriptData _ _ TxBodyNoScriptData = TxBodyNoScriptData
recomputeScriptData i f (TxBodyScriptData era dats (Ledger.Redeemers rdmrs)) =
  TxBodyScriptData era dats
    (Ledger.Redeemers $ Map.mapKeys updatePtr $ Map.filterWithKey idxFilter rdmrs)
  where -- updatePtr = Ledger.hoistPlutusPurpose (\(Ledger.AsIndex ix) -> Ledger.AsIndex (f ix)) -- TODO: replace when hoistPlutusPurpose is available
        updatePtr = \case
          Ledger.ConwayMinting (Ledger.AsIndex ix) -> Ledger.ConwayMinting (Ledger.AsIndex (f ix))
          Ledger.ConwaySpending (Ledger.AsIndex ix) -> Ledger.ConwaySpending (Ledger.AsIndex (f ix))
          Ledger.ConwayRewarding (Ledger.AsIndex ix) -> Ledger.ConwayRewarding (Ledger.AsIndex (f ix))
          Ledger.ConwayCertifying (Ledger.AsIndex ix) -> Ledger.ConwayCertifying (Ledger.AsIndex (f ix))
          Ledger.ConwayVoting (Ledger.AsIndex ix) -> Ledger.ConwayVoting (Ledger.AsIndex (f ix))
          Ledger.ConwayProposing (Ledger.AsIndex ix) -> Ledger.ConwayProposing (Ledger.AsIndex (f ix))
        idxFilter (Ledger.ConwaySpending (Ledger.AsIndex idx)) _ = Just idx /= i
        idxFilter (Ledger.ConwayMinting (Ledger.AsIndex idx)) _ = Just idx /= i
        idxFilter (Ledger.ConwayCertifying (Ledger.AsIndex idx)) _ = Just idx /= i
        idxFilter (Ledger.ConwayRewarding (Ledger.AsIndex idx)) _ = Just idx /= i
        idxFilter (Ledger.ConwayVoting (Ledger.AsIndex idx)) _ = Just idx /= i
        idxFilter (Ledger.ConwayProposing (Ledger.AsIndex idx)) _ = Just idx /= i

emptyTxBodyScriptData :: TxBodyScriptData Era
emptyTxBodyScriptData = TxBodyScriptData AlonzoEraOnwardsConway (Ledger.TxDats mempty) (Ledger.Redeemers mempty)

addScriptData :: Word32
              -> Ledger.Data (ShelleyLedgerEra Era)
              -> (Ledger.Data (ShelleyLedgerEra Era), Ledger.ExUnits)
              -> TxBodyScriptData Era
              -> TxBodyScriptData Era
addScriptData ix dat rdmr TxBodyNoScriptData = addScriptData ix dat rdmr emptyTxBodyScriptData
addScriptData ix dat rdmr (TxBodyScriptData era (Ledger.TxDats dats) (Ledger.Redeemers rdmrs)) =
  TxBodyScriptData era (Ledger.TxDats $ Map.insert (Ledger.hashData dat) dat dats)
                       (Ledger.Redeemers $ Map.insert (Ledger.ConwaySpending (Ledger.AsIndex ix)) rdmr rdmrs)

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
  TxOutDatumInTx s d    -> TxOutDatumHash s (hashScriptDataBytes d)
  TxOutDatumInline s sd -> TxOutDatumInline s sd

-- | Convert ScriptData to a `Test.QuickCheck.ContractModel.ThreatModel.Datum`.
txOutDatum :: ScriptData -> TxOutDatum CtxTx Era
txOutDatum d = TxOutDatumInTx AlonzoEraOnwardsConway (unsafeHashableScriptData d)

-- | Convert a Haskell value to ScriptData for use as a
-- `Test.QuickCheck.ContractModel.ThreatModel.Redeemer` or convert to a
-- `Test.QuickCheck.ContractModel.ThreatModel.Datum` with `txOutDatum`.
toScriptData :: ToData a => a -> ScriptData
toScriptData = fromPlutusData . toData

-- | Used for new inputs.
dummyTxId :: TxId
dummyTxId =
  fromShelleyTxId
  $ Ledger.txIdTxBody @LedgerEra
  $ Ledger.mkBasicTxBody

makeTxOut :: AddressAny -> Value -> TxOutDatum CtxTx Era -> ReferenceScript Era -> TxOut CtxUTxO Era
makeTxOut addr value datum refScript =
  toCtxUTxOTxOut $ TxOut (anyAddressInShelleyBasedEra shelleyBasedEra addr)
                         (TxOutValueShelleyBased shelleyBasedEra (toMaryValue value))
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

txReferenceInputs :: Tx Era -> [TxIn]
txReferenceInputs (Tx (TxBody body) _) =
  case txInsReference body of
    TxInsReferenceNone     -> []
    TxInsReference _ txins -> txins

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
validateTx :: LedgerProtocolParameters Era -> Tx Era -> UTxO Era -> ValidityReport
validateTx pparams tx utxos = case result of
  Left e -> ValidityReport False [show e]
  Right report -> ValidityReport (all isRight (Map.elems report))
                                 [show e | Left e <- Map.elems report]
  where
    result = evaluateTransactionExecutionUnits
                ConwayEra
                systemStart
                (toLedgerEpochInfo eraHistory)
                pparams
                utxos
                (getTxBody tx)
    eraHistory :: EraHistory
    eraHistory = EraHistory (mkInterpreter summary)

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
  UTxO $ Map.filterWithKey (\ k _ -> k `elem` map fst txIns
                                  || k `elem` toInputList txInsReference
                           ) utxo
  where
    toInputList (TxInsReference _ ins) = ins
    toInputList _ = []

convValidityInterval
  :: (TxValidityLowerBound era, TxValidityUpperBound era)
  -> ValidityInterval
convValidityInterval (lowerBound, upperBound) =
  ValidityInterval
    { invalidBefore = case lowerBound of
                        TxValidityNoLowerBound   -> SNothing
                        TxValidityLowerBound _ s -> SJust s
    , invalidHereafter = case upperBound of
                           TxValidityUpperBound _ Nothing -> SNothing
                           TxValidityUpperBound _ (Just s) -> SJust s
    }

