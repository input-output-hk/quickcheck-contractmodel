{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Test.QuickCheck.ThreatModel.TxModifier where

import Cardano.Api
import Cardano.Api.Shelley
import Cardano.Ledger.Alonzo.Scripts qualified as Ledger
import Cardano.Ledger.Alonzo.TxWits qualified as Ledger
import Cardano.Ledger.Alonzo.TxBody qualified as Ledger
import Cardano.Ledger.Conway.Scripts qualified as Ledger
import Cardano.Ledger.Conway.TxBody qualified as Ledger
import Cardano.Ledger.Binary qualified as CBOR
import Cardano.Ledger.Api.Era (eraProtVerLow)
import Data.Coerce

import Data.Map qualified as Map
import Data.Maybe
import Data.Maybe.Strict
import Data.Sequence.Strict qualified as Seq
import Data.Set qualified as Set

import Test.QuickCheck.ThreatModel.Cardano.Api

-- | A transaction output paired with its index in the transaction.
data Output = Output { outputTxOut :: TxOut CtxTx Era
                     , outputIx    :: TxIx }
  deriving Show


-- | A transaction input reference togheter with the corresponding `TxOut` from the `UTxO` set.
data Input = Input { inputTxOut :: TxOut CtxUTxO Era
                   , inputTxIn  :: TxIn }
  deriving Show

-- | Functions common to both `Input`s and `Output`s.
class IsInputOrOutput t where
  -- | Change the target address of an input or an output. For outputs this means redirecting an
  --   output to a different address, and for inputs it means modifying the UTxO set, changing the
  --   owner of the given input.
  --
  --   /Note: Does not work for script inputs./
  changeAddressOf :: t -> AddressAny -> TxModifier

  -- | Change the value of an input or an output.
  changeValueOf   :: t -> Value -> TxModifier

  -- | Change the datum on an input or an output.
  changeDatumOf   :: t -> Datum -> TxModifier

  -- | Change the reference script on an input or an output
  changeRefScriptOf :: t -> ReferenceScript Era -> TxModifier

  -- | Get the address (pubkey or script address) of an input or an output.
  addressOf       :: t -> AddressAny

  -- | Get the value at an input or an output.
  valueOf         :: t -> Value

  -- | Get the reference script at an input or an output.
  refScriptOf     :: t -> ReferenceScript Era

instance IsInputOrOutput Output where
  changeAddressOf o a   = txMod $ ChangeOutput (outputIx o) (Just a) Nothing Nothing Nothing
  changeValueOf o v     = txMod $ ChangeOutput (outputIx o) Nothing (Just v) Nothing Nothing
  changeDatumOf o d     = txMod $ ChangeOutput (outputIx o) Nothing Nothing (Just d) Nothing
  changeRefScriptOf o r = txMod $ ChangeOutput (outputIx o) Nothing Nothing Nothing (Just r)
  addressOf             = addressOfTxOut . outputTxOut
  valueOf               = valueOfTxOut . outputTxOut
  refScriptOf           = referenceScriptOfTxOut . outputTxOut

instance IsInputOrOutput Input where
  changeAddressOf i a
    | isKeyAddressAny (addressOf i) = txMod $ ChangeInput (inputTxIn i) (Just a) Nothing Nothing Nothing
    | otherwise                     = error "Cannot changeAddressOf ScriptInput"
  changeValueOf i v
    | isKeyAddressAny (addressOf i) = txMod $ ChangeInput (inputTxIn i) Nothing (Just v) Nothing Nothing
    | otherwise                     = txMod $ ChangeScriptInput (inputTxIn i) (Just v) Nothing Nothing Nothing
  changeDatumOf i d
    | isKeyAddressAny (addressOf i) = txMod $ ChangeInput (inputTxIn i) Nothing Nothing (Just d) Nothing
    | otherwise                     = txMod $ ChangeScriptInput (inputTxIn i) Nothing (Just d) Nothing Nothing
  changeRefScriptOf i r
    | isKeyAddressAny (addressOf i) = txMod $ ChangeInput (inputTxIn i) Nothing Nothing Nothing (Just r)
    | otherwise                     = txMod $ ChangeScriptInput (inputTxIn i) Nothing Nothing Nothing (Just r)
  addressOf   = addressOfTxOut . inputTxOut
  valueOf     = valueOfTxOut   . inputTxOut
  refScriptOf = referenceScriptOfTxOut . inputTxOut

-- | Type synonym for datums. The `CtxTx` context means that the actual datum value can be present,
--   not just the hash.
type Datum    = TxOutDatum CtxTx Era

-- | Redeemers are plain `ScriptData`.
type Redeemer = ScriptData

-- | The type of transaction modifiers. When combined using the monoid instance, individual
--   modifications are applied in left-to-right order.
newtype TxModifier = TxModifier [TxMod]
  deriving newtype (Semigroup, Monoid)

data TxMod where

  RemoveInput    :: TxIn
                 -> TxMod

  RemoveOutput   :: TxIx
                 -> TxMod

  ChangeOutput :: TxIx
               -> Maybe AddressAny
               -> Maybe Value
               -> Maybe Datum
               -> Maybe (ReferenceScript Era)
               -> TxMod

  ChangeInput  :: TxIn
               -> Maybe AddressAny
               -> Maybe Value
               -> Maybe Datum
               -> Maybe (ReferenceScript Era)
               -> TxMod

  ChangeScriptInput :: TxIn
                    -> Maybe Value
                    -> Maybe Datum
                    -> Maybe Redeemer
                    -> Maybe (ReferenceScript Era)
                    -> TxMod

  ChangeValidityRange :: Maybe (TxValidityLowerBound Era)
                      -> Maybe (TxValidityUpperBound Era)
                      -> TxMod

  AddOutput :: AddressAny
            -> Value
            -> Datum
            -> ReferenceScript Era
            -> TxMod

  -- TODO: unify the `AddInput` constructors

  AddInput  :: AddressAny
            -> Value
            -> Datum
            -> ReferenceScript Era
            -> Bool -- isReferenceInput
            -> TxMod

  AddReferenceScriptInput :: ScriptHash
                          -> Value
                          -> Datum
                          -> Redeemer
                          -> TxMod

  AddPlutusScriptInput :: PlutusScript PlutusScriptV2
                       -> Value
                       -> Datum
                       -> Redeemer
                       -> ReferenceScript Era
                       -> TxMod

  AddPlutusScriptReferenceInput :: PlutusScript PlutusScriptV2
                                -> Value
                                -> Datum
                                -> ReferenceScript Era
                                -> TxMod

  AddSimpleScriptInput :: SimpleScript
                       -> Value
                       --TODO: -> Datum ??
                       -> ReferenceScript Era
                       -> Bool -- isReferenceInput
                       -> TxMod

  ReplaceTx :: Tx Era -> UTxO Era -> TxMod

  deriving stock (Show)

txMod :: TxMod -> TxModifier
txMod m = TxModifier [m]

applyTxModifier :: Tx Era -> UTxO Era -> TxModifier -> (Tx Era, UTxO Era)
applyTxModifier tx utxos (TxModifier ms) = foldl (uncurry applyTxMod) (tx, utxos) ms

mkNewTxIn :: UTxO Era -> TxIn
mkNewTxIn utxos = TxIn dummyTxId (TxIx txIx)
  where
    txIx = maximum $ 0 : map (+ 1) [ ix | TxIn txId (TxIx ix) <- Map.keys $ unUTxO utxos, txId == dummyTxId ]

applyTxMod :: Tx Era -> UTxO Era -> TxMod -> (Tx Era, UTxO Era)

applyTxMod tx utxos (ChangeValidityRange mlo mhi) =
    (Tx (ShelleyTxBody era body{Ledger.ctbVldt=validity'} scripts scriptData auxData scriptValidity) wits, utxos)
  where
    Tx bdy@(ShelleyTxBody era body scripts scriptData auxData scriptValidity) wits = tx
    TxBody TxBodyContent{txValidityLowerBound = lo, txValidityUpperBound = hi} = bdy
    validity' = convValidityInterval (fromMaybe lo mlo, fromMaybe hi mhi)

applyTxMod tx utxos (RemoveInput i) =
    (Tx (ShelleyTxBody era body' scripts scriptData' auxData validity) wits, utxos)
  where
    Tx (ShelleyTxBody era body@Ledger.ConwayTxBody{..} scripts scriptData auxData validity) wits = tx
    inputs' = Set.delete (toShelleyTxIn i) ctbSpendInputs
    refInputs' = Set.delete (toShelleyTxIn i) ctbReferenceInputs
    body' = body{ Ledger.ctbSpendInputs = inputs'
                , Ledger.ctbReferenceInputs = refInputs'
                }
    scriptData' = case Ledger.indexOf (Ledger.AsItem (toShelleyTxIn i)) ctbSpendInputs of
      SNothing  -> scriptData
      SJust (Ledger.AsIndex idx) -> recomputeScriptData (Just idx) idxUpdate scriptData
        where
          idxUpdate idx'
            | idx' > idx = idx' - 1
            | otherwise  = idx'

applyTxMod tx utxos (RemoveOutput (TxIx i)) =
    (Tx (ShelleyTxBody era body{Ledger.ctbOutputs = outputs'} scripts scriptData auxData validity) wits, utxos)
  where
    Tx (ShelleyTxBody era body@Ledger.ConwayTxBody{..} scripts scriptData auxData validity) wits = tx
    outputs' = case Seq.splitAt (fromIntegral i) ctbOutputs of
                 (before, _ Seq.:<| after) -> before <> after
                 (_, Seq.Empty)            -> error $ "RemoveOutput: Can't remove index " ++ show i ++ " from "
                                                   ++ show (Seq.length ctbOutputs) ++ " outputs"

applyTxMod tx utxos (AddOutput addr value datum refscript) =
    (Tx (ShelleyTxBody era body{Ledger.ctbOutputs = outputs'} scripts scriptData' auxData validity) wits, utxos)
  where
    Tx (ShelleyTxBody era body@Ledger.ConwayTxBody{..} scripts scriptData auxData validity) wits = tx
    outputs' = ctbOutputs Seq.:|> CBOR.mkSized (eraProtVerLow @LedgerEra) out
    out = toShelleyTxOut shelleyBasedEra
                         (makeTxOut addr value datum refscript)
    scriptData' = case datum of
      TxOutDatumNone       -> scriptData
      TxOutDatumHash{}     -> scriptData
      TxOutDatumInTx _ d   -> addDatum (toAlonzoData d) scriptData
      TxOutDatumInline _ d -> addDatum (toAlonzoData d) scriptData

applyTxMod tx utxos (AddInput addr value datum rscript False) =
    ( Tx (ShelleyTxBody era body{Ledger.ctbSpendInputs = inputs'} scripts scriptData'' auxData validity) wits
    , utxos' )
  where
    Tx (ShelleyTxBody era body@Ledger.ConwayTxBody{..} scripts scriptData auxData validity) wits = tx
    txIn = mkNewTxIn utxos

    input   = toShelleyTxIn txIn
    inputs' = Set.insert input ctbSpendInputs
    SJust (Ledger.AsIndex idx) = Ledger.indexOf (Ledger.AsItem input) inputs'

    txOut   = makeTxOut addr value datum rscript
    utxos'  = UTxO . Map.insert txIn txOut . unUTxO $ utxos

    idxUpdate idx'
      | idx' >= idx = idx' + 1
      | otherwise   = idx'

    scriptData'' = case datum of
      TxOutDatumNone       -> scriptData'
      TxOutDatumHash{}     -> scriptData'
      TxOutDatumInTx _ d   -> addDatum (toAlonzoData d) scriptData'
      TxOutDatumInline _ d -> addDatum (toAlonzoData d) scriptData'

    scriptData' = recomputeScriptData Nothing idxUpdate scriptData

applyTxMod tx utxos (AddInput addr value datum rscript True) =
    ( Tx (ShelleyTxBody era body{Ledger.ctbReferenceInputs = refInputs} scripts scriptData auxData validity) wits
    , utxos' )
  where
    Tx (ShelleyTxBody era body@Ledger.ConwayTxBody{..} scripts scriptData auxData validity) wits = tx

    txIn = mkNewTxIn utxos

    input   = toShelleyTxIn txIn
    refInputs = Set.insert input ctbReferenceInputs

    txOut   = makeTxOut addr value datum rscript
    utxos'  = UTxO . Map.insert txIn txOut . unUTxO $ utxos

applyTxMod tx utxos (AddPlutusScriptReferenceInput script value datum rscript) =
    ( Tx (ShelleyTxBody era body{Ledger.ctbReferenceInputs = refInputs'} scripts' scriptData auxData validity) wits
    , utxos' )
  where
    Tx (ShelleyTxBody era body@Ledger.ConwayTxBody{..} scripts scriptData auxData validity) wits = tx

    txIn       = mkNewTxIn utxos
    input      = toShelleyTxIn txIn
    refInputs' = Set.insert input ctbReferenceInputs

    txOut  = makeTxOut addr value datum rscript
    utxos' = UTxO . Map.insert txIn txOut . unUTxO $ utxos

    scriptInEra = ScriptInEra PlutusScriptV2InConway
                  (PlutusScript PlutusScriptV2 script)
    newScript = toShelleyScript @Era scriptInEra
    scripts'  = scripts ++ [newScript]

    hash = hashScript $ PlutusScript PlutusScriptV2 script
    addr = scriptAddressAny hash

applyTxMod tx utxos (AddReferenceScriptInput script value datum redeemer) =
    ( Tx (ShelleyTxBody era body{Ledger.ctbSpendInputs = inputs'} scripts scriptData' auxData validity) wits
    , utxos' )
  where
    Tx (ShelleyTxBody era body@Ledger.ConwayTxBody{..} scripts scriptData auxData validity) wits = tx

    txIn = mkNewTxIn utxos
    input  = toShelleyTxIn txIn
    inputs' = Set.insert input ctbSpendInputs

    txOut  = makeTxOut addr value datum ReferenceScriptNone
    utxos' = UTxO . Map.insert txIn txOut . unUTxO $ utxos

    SJust (Ledger.AsIndex idx) = Ledger.indexOf (Ledger.AsItem input) inputs'
    idxUpdate idx'
      | idx' >= idx = idx' + 1
      | otherwise   = idx'

    datum' = case datum of
      TxOutDatumNone       -> error "Bad test!"
      TxOutDatumHash{}     -> error "Bad test!"
      TxOutDatumInTx _ d   -> toAlonzoData d
      TxOutDatumInline _ d -> toAlonzoData d

    scriptData' = addScriptData idx datum' (toAlonzoData $ unsafeHashableScriptData redeemer, toAlonzoExUnits $ ExecutionUnits 0 0)
                $ recomputeScriptData Nothing idxUpdate scriptData

    addr = scriptAddressAny script

applyTxMod tx utxos (AddPlutusScriptInput script value datum redeemer rscript) =
    ( Tx (ShelleyTxBody era body{Ledger.ctbSpendInputs = inputs'} scripts' scriptData' auxData validity) wits
    , utxos' )
  where
    Tx (ShelleyTxBody era body@Ledger.ConwayTxBody{..} scripts scriptData auxData validity) wits = tx

    txIn = mkNewTxIn utxos
    input  = toShelleyTxIn txIn
    inputs' = Set.insert input ctbSpendInputs

    txOut  = makeTxOut addr value datum rscript
    utxos' = UTxO . Map.insert txIn txOut . unUTxO $ utxos

    scriptInEra = ScriptInEra PlutusScriptV2InConway
                  (PlutusScript PlutusScriptV2 script)
    newScript = toShelleyScript @Era scriptInEra
    scripts'  = scripts ++ [newScript]

    SJust (Ledger.AsIndex idx) = Ledger.indexOf (Ledger.AsItem input) inputs'
    idxUpdate idx'
      | idx' >= idx = idx' + 1
      | otherwise   = idx'

    datum' = case datum of
      TxOutDatumNone       -> error "Bad test!"
      TxOutDatumHash{}     -> error "Bad test!"
      TxOutDatumInTx _ d   -> toAlonzoData d
      TxOutDatumInline _ d -> toAlonzoData d

    scriptData' = addScriptData idx datum' (toAlonzoData $ unsafeHashableScriptData redeemer, toAlonzoExUnits $ ExecutionUnits 0 0)
                $ recomputeScriptData Nothing idxUpdate scriptData

    hash = hashScript $ PlutusScript PlutusScriptV2 script
    addr = scriptAddressAny hash

applyTxMod tx utxos (AddSimpleScriptInput script value rscript False) =
    ( Tx (ShelleyTxBody era body{Ledger.ctbSpendInputs = inputs'} scripts' scriptData' auxData validity) wits
    , utxos' )
  where
    Tx (ShelleyTxBody era body@Ledger.ConwayTxBody{..} scripts scriptData auxData validity) wits = tx

    txIn = mkNewTxIn utxos

    input  = toShelleyTxIn txIn
    inputs' = Set.insert input ctbSpendInputs

    txOut  = makeTxOut addr value TxOutDatumNone rscript
    utxos' = UTxO . Map.insert txIn txOut . unUTxO $ utxos

    scriptInEra = ScriptInEra SimpleScriptInConway
                  (SimpleScript script)
    newScript = toShelleyScript @Era scriptInEra
    scripts'  = scripts ++ [newScript]

    SJust (Ledger.AsIndex idx) = Ledger.indexOf (Ledger.AsItem input) inputs'
    idxUpdate idx'
      | idx' >= idx = idx' + 1
      | otherwise   = idx'

    scriptData' = recomputeScriptData Nothing idxUpdate scriptData

    addr = scriptAddressAny $ hashScript (SimpleScript script)

-- NOTE: this is okay (??) because there is no requirement to provide the
-- data for reference inputs
applyTxMod tx utxos (AddSimpleScriptInput script value rscript True) =
    ( Tx (ShelleyTxBody era body{Ledger.ctbReferenceInputs = refInputs} scripts' scriptData auxData validity) wits
    , utxos' )
  where
    Tx (ShelleyTxBody era body@Ledger.ConwayTxBody{..} scripts scriptData auxData validity) wits = tx

    txIn = mkNewTxIn utxos
    input  = toShelleyTxIn txIn
    refInputs = Set.insert input ctbReferenceInputs

    txOut  = makeTxOut addr value TxOutDatumNone rscript
    utxos' = UTxO . Map.insert txIn txOut . unUTxO $ utxos

    scriptInEra = ScriptInEra SimpleScriptInConway
                  (SimpleScript script)
    newScript = toShelleyScript @Era scriptInEra
    scripts'  = scripts ++ [newScript]

    addr = scriptAddressAny $ hashScript (SimpleScript script)

applyTxMod tx utxos (ChangeOutput ix maddr mvalue mdatum mrscript) =
    (Tx (ShelleyTxBody era body{Ledger.ctbOutputs = outputs'} scripts scriptData' auxData validity) wits, utxos)
  where
    TxIx (fromIntegral -> idx) = ix
    Tx bdy@(ShelleyTxBody era body@Ledger.ConwayTxBody{..} scripts scriptData auxData validity) wits = tx
    TxBody (TxBodyContent{txOuts=txOuts}) = bdy
    TxOut (AddressInEra _ (toAddressAny -> addr)) (txOutValueToValue -> value) datum rscript = txOuts !! idx
    (outputsStart, _ Seq.:<| outputsEnd) = Seq.splitAt idx ctbOutputs
    outputs' = outputsStart Seq.>< (CBOR.mkSized (eraProtVerLow @LedgerEra) out Seq.:<| outputsEnd)
    out = toShelleyTxOut shelleyBasedEra $ makeTxOut (fromMaybe addr maddr)
                                                     (fromMaybe value mvalue)
                                                     (fromMaybe datum mdatum)
                                                     (fromMaybe rscript mrscript)

    scriptData' = case mdatum of
      Nothing -> scriptData
      Just d -> case d of
        TxOutDatumNone       -> scriptData
        TxOutDatumHash{}     -> scriptData
        TxOutDatumInTx _ d   -> addDatum (toAlonzoData d) scriptData
        TxOutDatumInline _ d -> addDatum (toAlonzoData d) scriptData


applyTxMod tx utxos (ChangeInput txIn maddr mvalue mdatum mrscript) =
    (Tx (ShelleyTxBody era body scripts scriptData' auxData validity) wits, utxos')
  where
    Tx (ShelleyTxBody era body scripts scriptData auxData validity) wits = tx
    (addr, value, utxoDatum, rscript) = case Map.lookup txIn $ unUTxO utxos of
      Just (TxOut (AddressInEra _ (toAddressAny -> addr)) (txOutValueToValue -> value) datum rscript) ->
        (addr, value, datum, rscript)
      Nothing -> error $ "Index " ++ show txIn ++ " doesn't exist."

    txOut = TxOut (anyAddressInShelleyBasedEra shelleyBasedEra (fromMaybe addr maddr))
                  (TxOutValueShelleyBased shelleyBasedEra $ toMaryValue $ fromMaybe value mvalue)
                  (fromMaybe utxoDatum $ toCtxUTxODatum <$> mdatum)
                  (fromMaybe rscript mrscript)
    utxos' = UTxO . Map.insert txIn txOut . unUTxO $ utxos

    scriptData' = case mdatum of
      Nothing                     -> scriptData
      Just TxOutDatumNone         -> scriptData
      Just TxOutDatumHash{}       -> scriptData
      Just (TxOutDatumInTx _ d)   -> addDatum (toAlonzoData d) scriptData
      Just (TxOutDatumInline _ d) -> addDatum (toAlonzoData d) scriptData

applyTxMod tx utxos (ChangeScriptInput txIn mvalue mdatum mredeemer mrscript) =
    (Tx (ShelleyTxBody era body scripts scriptData' auxData validity) wits, utxos')
  where
    Tx (ShelleyTxBody era body@Ledger.ConwayTxBody{..} scripts scriptData auxData validity) wits = tx
    (addr, value, utxoDatum, rscript) = case Map.lookup txIn $ unUTxO utxos of
      Just (TxOut addr (txOutValueToValue -> value) utxoDatum rscript) ->
        (addr, value, utxoDatum, rscript)
      Nothing -> error $ "The index " ++ show txIn ++ " doesn't exist."

    (datum, (redeemer, exunits)) = case scriptData of
      TxBodyNoScriptData -> error "No script data available"
      TxBodyScriptData _ (Ledger.TxDats dats) (Ledger.Redeemers rdmrs) ->
        (fromJust $ Map.lookup utxoDatumHash dats,
         fromJust $ Map.lookup (Ledger.ConwaySpending (Ledger.AsIndex idx)) rdmrs)

    utxoDatumHash = case utxoDatum of
      TxOutDatumNone       -> error "No existing datum"
      TxOutDatumInline _ d -> coerce $ hashScriptDataBytes d
      TxOutDatumHash _ h   -> coerce h

    adatum = case mdatum of
      Just TxOutDatumNone         -> error "Bad test!"
      Just TxOutDatumHash{}       -> error "Bad test!"
      Just (TxOutDatumInTx _ d)   -> toAlonzoData d
      Just (TxOutDatumInline _ d) -> toAlonzoData d
      Nothing                     -> datum

    txOut = TxOut addr
                  (TxOutValueShelleyBased shelleyBasedEra $ toMaryValue $ fromMaybe value mvalue)
                  (fromMaybe utxoDatum $ toCtxUTxODatum <$> mdatum)
                  (fromMaybe rscript mrscript)

    utxos' = UTxO . Map.insert txIn txOut . unUTxO $ utxos

    idx = case Ledger.indexOf (Ledger.AsItem (toShelleyTxIn txIn)) ctbSpendInputs of
      SJust (Ledger.AsIndex idx) -> idx
      _                          -> error "The impossible happened!"

    scriptData' = addScriptData idx adatum
                                    (fromMaybe redeemer (toAlonzoData . unsafeHashableScriptData <$> mredeemer), exunits)
                                    scriptData

applyTxMod _ _ (ReplaceTx tx utxos) = (tx, utxos)

-- | Add a new output of any type (public key or script)
addOutput :: AddressAny -> Value -> Datum -> ReferenceScript Era -> TxModifier
addOutput addr value datum refscript = txMod $ AddOutput addr value datum refscript

-- | Remove an output of any type.
removeOutput :: Output -> TxModifier
removeOutput output = txMod $ RemoveOutput $ outputIx output

-- | Add a new public key input.
addKeyInput :: AddressAny -> Value -> Datum -> ReferenceScript Era -> TxModifier
addKeyInput addr value datum rscript = txMod $ AddInput addr value datum rscript False

-- | Add a new public key reference input.
addKeyReferenceInput :: AddressAny -> Value -> Datum -> ReferenceScript Era -> TxModifier
addKeyReferenceInput addr value datum rscript = txMod $ AddInput addr value datum rscript True

-- | Remove an input of any type.
removeInput :: Input -> TxModifier
removeInput inp = txMod $ RemoveInput $ inputTxIn inp

-- | Add a reference script input
addReferenceScriptInput :: ScriptHash -> Value -> Datum -> Redeemer -> TxModifier
addReferenceScriptInput script value datum redeemer = txMod $ AddReferenceScriptInput script value datum redeemer

-- | Add a plutus script input.
addPlutusScriptInput :: PlutusScript PlutusScriptV2 -> Value -> Datum -> Redeemer -> ReferenceScript Era -> TxModifier
addPlutusScriptInput script value datum redeemer rscript = txMod $ AddPlutusScriptInput script value datum redeemer rscript

-- | Add a plutus script reference input.
addPlutusScriptReferenceInput :: PlutusScript PlutusScriptV2 -> Value -> Datum -> ReferenceScript Era -> TxModifier
addPlutusScriptReferenceInput script value datum rscript = txMod $ AddPlutusScriptReferenceInput script value datum rscript

-- | Add a simple script input.
addSimpleScriptInput :: SimpleScript -> Value -> ReferenceScript Era -> TxModifier
addSimpleScriptInput script value rscript = txMod $ AddSimpleScriptInput script value rscript False

-- | Add a simple script reference input.
addSimpleScriptReferenceInput :: SimpleScript -> Value -> ReferenceScript Era -> TxModifier
addSimpleScriptReferenceInput script value rscript = txMod $ AddSimpleScriptInput script value rscript True

-- | Change the redeemer of a script input.
changeRedeemerOf :: Input -> Redeemer -> TxModifier
changeRedeemerOf i r
  | isKeyAddressAny (addressOf i) = error "Cannot changeRedeemerOf public key input"
  | otherwise                     = txMod $ ChangeScriptInput (inputTxIn i) Nothing Nothing (Just r) Nothing

-- | Change the validity range of the transaction.
changeValidityRange :: (TxValidityLowerBound Era, TxValidityUpperBound Era) -> TxModifier
changeValidityRange (lo, hi) = txMod $ ChangeValidityRange (Just lo) (Just hi)

-- | Change the validity lower bound of the transaction.
changeValidityLowerBound :: TxValidityLowerBound Era -> TxModifier
changeValidityLowerBound lo = txMod $ ChangeValidityRange (Just lo) Nothing

-- | Change the validity upper bound of the transaction.
changeValidityUpperBound :: TxValidityUpperBound Era -> TxModifier
changeValidityUpperBound hi = txMod $ ChangeValidityRange Nothing (Just hi)

-- | The most general transaction modifier. Simply replace the original transaction and `UTxO` set
--   by the given values. In most cases the modifiers above should be sufficient.
replaceTx :: Tx Era -> UTxO Era -> TxModifier
replaceTx tx utxos = txMod $ ReplaceTx tx utxos

