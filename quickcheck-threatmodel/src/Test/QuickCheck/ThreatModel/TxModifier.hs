{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Test.QuickCheck.ThreatModel.TxModifier where

import Cardano.Api
import Cardano.Api.Shelley
import Cardano.Ledger.Alonzo.Tx qualified as Ledger (indexOf)
import Cardano.Ledger.Alonzo.TxWitness qualified as Ledger
import Cardano.Ledger.Babbage.TxBody qualified as Ledger
import Cardano.Ledger.Serialization qualified as CBOR
import Data.Coerce

import Cardano.Ledger.Alonzo.Scripts qualified as Ledger
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

  -- | Get the address (pubkey or script address) of an input or an output.
  addressOf       :: t -> AddressAny

  -- | Get the value at an input or an output.
  valueOf         :: t -> Value

instance IsInputOrOutput Output where
  changeAddressOf o a = txMod $ ChangeOutput (outputIx o) (Just a) Nothing Nothing
  changeValueOf o v   = txMod $ ChangeOutput (outputIx o) Nothing (Just v) Nothing
  changeDatumOf o d   = txMod $ ChangeOutput (outputIx o) Nothing Nothing (Just d)
  addressOf           = addressOfTxOut . outputTxOut
  valueOf             = valueOfTxOut . outputTxOut

instance IsInputOrOutput Input where
  changeAddressOf i a
    | isKeyAddressAny (addressOf i) = txMod $ ChangeInput (inputTxIn i) (Just a) Nothing Nothing
    | otherwise                     = error "Cannot changeAddressOf ScriptInput"
  changeValueOf i v
    | isKeyAddressAny (addressOf i) = txMod $ ChangeInput (inputTxIn i) Nothing (Just v) Nothing
    | otherwise                     = txMod $ ChangeScriptInput (inputTxIn i) (Just v) Nothing Nothing
  changeDatumOf i d
    | isKeyAddressAny (addressOf i) = txMod $ ChangeInput (inputTxIn i) Nothing Nothing (Just d)
    | otherwise                     = txMod $ ChangeScriptInput (inputTxIn i) Nothing (Just d) Nothing
  addressOf = addressOfTxOut . inputTxOut
  valueOf   = valueOfTxOut   . inputTxOut

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

  ChangeOutput :: TxIx -> Maybe AddressAny -> Maybe Value -> Maybe Datum -> TxMod
  ChangeInput  :: TxIn -> Maybe AddressAny -> Maybe Value -> Maybe Datum -> TxMod

  ChangeScriptInput :: TxIn
                    -> Maybe Value
                    -> Maybe Datum
                    -> Maybe Redeemer
                    -> TxMod

  ChangeValidityRange :: Maybe (TxValidityLowerBound Era)
                      -> Maybe (TxValidityUpperBound Era)
                      -> TxMod

  AddOutput :: AddressAny -> Value -> Datum -> TxMod
  AddInput  :: AddressAny -> Value -> Datum -> TxMod

  AddPlutusScriptInput :: PlutusScript PlutusScriptV2
                       -> Value
                       -> Datum
                       -> Redeemer
                       -> TxMod


  AddSimpleScriptInput :: SimpleScript SimpleScriptV2
                       -> Value
                       -> TxMod

  ReplaceTx :: Tx Era -> UTxO Era -> TxMod

  deriving stock (Show)

txMod :: TxMod -> TxModifier
txMod m = TxModifier [m]

applyTxModifier :: Tx Era -> UTxO Era -> TxModifier -> (Tx Era, UTxO Era)
applyTxModifier tx utxos (TxModifier ms) = foldl (uncurry applyTxMod) (tx, utxos) ms

applyTxMod :: Tx Era -> UTxO Era -> TxMod -> (Tx Era, UTxO Era)

applyTxMod tx utxos (ChangeValidityRange mlo mhi) =
    (Tx (ShelleyTxBody era body{Ledger.txvldt=validity'} scripts scriptData auxData scriptValidity) wits, utxos)
  where
    Tx bdy@(ShelleyTxBody era body scripts scriptData auxData scriptValidity) wits = tx
    TxBody TxBodyContent{txValidityRange = (lo, hi)} = bdy
    validity' = convValidityInterval (fromMaybe lo mlo, fromMaybe hi mhi)

applyTxMod tx utxos (RemoveInput i) =
    (Tx (ShelleyTxBody era body{Ledger.inputs = inputs'} scripts scriptData' auxData validity) wits, utxos)
  where
    Tx (ShelleyTxBody era body@Ledger.TxBody{..} scripts scriptData auxData validity) wits = tx
    inputs' = Set.delete (toShelleyTxIn i) inputs
    SJust idx = Ledger.indexOf (toShelleyTxIn i) inputs
    idxUpdate idx'
      | idx' > idx = idx' - 1
      | otherwise  = idx'
    scriptData' = recomputeScriptData (Just idx) idxUpdate scriptData

applyTxMod tx utxos (RemoveOutput (TxIx i)) =
    (Tx (ShelleyTxBody era body{Ledger.outputs = outputs'} scripts scriptData auxData validity) wits, utxos)
  where
    Tx (ShelleyTxBody era body@Ledger.TxBody{..} scripts scriptData auxData validity) wits = tx
    outputs' = case Seq.splitAt (fromIntegral i) outputs of
                 (before, _ Seq.:<| after) -> before <> after
                 (_, Seq.Empty)            -> error $ "RemoveOutput: Can't remove index " ++ show i ++ " from "
                                                   ++ show (Seq.length outputs) ++ " outputs"

applyTxMod tx utxos (AddOutput addr value datum) =
    (Tx (ShelleyTxBody era body{Ledger.outputs = outputs'} scripts scriptData' auxData validity) wits, utxos)
  where
    Tx (ShelleyTxBody era body@Ledger.TxBody{..} scripts scriptData auxData validity) wits = tx
    outputs' = outputs Seq.:|> CBOR.mkSized out
    out = toShelleyTxOut shelleyBasedEra (makeTxOut addr value datum ReferenceScriptNone)
    scriptData' = case datum of
      TxOutDatumNone       -> scriptData
      TxOutDatumHash{}     -> scriptData
      TxOutDatumInTx _ d   -> addDatum (toAlonzoData d) scriptData
      TxOutDatumInline _ d -> addDatum (toAlonzoData d) scriptData

applyTxMod tx utxos (AddInput addr value datum) =
    ( Tx (ShelleyTxBody era body{Ledger.inputs = inputs'} scripts scriptData'' auxData validity) wits
    , utxos' )
  where
    Tx (ShelleyTxBody era body@Ledger.TxBody{..} scripts scriptData auxData validity) wits = tx

    txIn    = TxIn dummyTxId (TxIx txIx)
    txIx    = maximum $ 0 : map (+ 1) [ ix | TxIn txId (TxIx ix) <- Map.keys $ unUTxO utxos, txId == dummyTxId ]
    input   = toShelleyTxIn txIn
    inputs' = Set.insert input inputs
    SJust idx = Ledger.indexOf input inputs'

    txOut   = makeTxOut addr value datum ReferenceScriptNone
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

applyTxMod tx utxos (AddPlutusScriptInput script value datum redeemer) =
    ( Tx (ShelleyTxBody era body{Ledger.inputs = inputs'} scripts' scriptData' auxData validity) wits
    , utxos' )
  where
    Tx (ShelleyTxBody era body@Ledger.TxBody{..} scripts scriptData auxData validity) wits = tx

    txIx   = maximum $ 0 : map (+ 1) [ ix | TxIn txId (TxIx ix) <- Map.keys $ unUTxO utxos, txId == dummyTxId ]
    txIn   = TxIn dummyTxId (TxIx txIx)
    input  = toShelleyTxIn txIn
    inputs' = Set.insert input inputs

    txOut  = makeTxOut addr value datum ReferenceScriptNone
    utxos' = UTxO . Map.insert txIn txOut . unUTxO $ utxos

    scriptInEra = ScriptInEra PlutusScriptV2InBabbage
                  (PlutusScript PlutusScriptV2 script)
    newScript = toShelleyScript @Era scriptInEra
    scripts'  = scripts ++ [newScript]

    SJust idx = Ledger.indexOf input inputs'
    idxUpdate idx'
      | idx' >= idx = idx' + 1
      | otherwise   = idx'

    datum' = case datum of
      TxOutDatumNone       -> error "Bad test!"
      TxOutDatumHash{}     -> error "Bad test!"
      TxOutDatumInTx _ d   -> toAlonzoData d
      TxOutDatumInline _ d -> toAlonzoData d

    scriptData' = addScriptData idx datum' (toAlonzoData redeemer, toAlonzoExUnits $ ExecutionUnits 0 0)
                $ recomputeScriptData Nothing idxUpdate scriptData

    hash = hashScript $ PlutusScript PlutusScriptV2 script
    addr = scriptAddressAny hash

applyTxMod tx utxos (AddSimpleScriptInput script value) =
    ( Tx (ShelleyTxBody era body{Ledger.inputs = inputs'} scripts' scriptData' auxData validity) wits
    , utxos' )
  where
    Tx (ShelleyTxBody era body@Ledger.TxBody{..} scripts scriptData auxData validity) wits = tx

    txIx   = maximum $ 0 : map (+ 1) [ ix | TxIn txId (TxIx ix) <- Map.keys $ unUTxO utxos, txId == dummyTxId ]
    txIn   = TxIn dummyTxId (TxIx txIx)
    input  = toShelleyTxIn txIn
    inputs' = Set.insert input inputs

    txOut  = makeTxOut addr value TxOutDatumNone ReferenceScriptNone
    utxos' = UTxO . Map.insert txIn txOut . unUTxO $ utxos

    scriptInEra = ScriptInEra SimpleScriptV2InBabbage
                  (SimpleScript SimpleScriptV2 script)
    newScript = toShelleyScript @Era scriptInEra
    scripts'  = scripts ++ [newScript]

    SJust idx = Ledger.indexOf input inputs'
    idxUpdate idx'
      | idx' >= idx = idx' + 1
      | otherwise   = idx'

    scriptData' = recomputeScriptData Nothing idxUpdate scriptData

    addr = scriptAddressAny $ hashScript (SimpleScript SimpleScriptV2 script)

applyTxMod tx utxos (ChangeOutput ix maddr mvalue mdatum) =
    (Tx (ShelleyTxBody era body{Ledger.outputs = outputs'} scripts scriptData' auxData validity) wits, utxos)
  where
    TxIx (fromIntegral -> idx) = ix
    Tx bdy@(ShelleyTxBody era body@Ledger.TxBody{..} scripts scriptData auxData validity) wits = tx
    TxBody (TxBodyContent{txOuts=txOuts}) = bdy
    TxOut (AddressInEra _ (toAddressAny -> addr)) (txOutValueToValue -> value) datum rscript = txOuts !! idx
    (outputsStart, _ Seq.:<| outputsEnd) = Seq.splitAt idx outputs
    outputs' = outputsStart Seq.>< (CBOR.mkSized out Seq.:<| outputsEnd)
    out = toShelleyTxOut shelleyBasedEra (makeTxOut (fromMaybe addr maddr)
                                                    (fromMaybe value mvalue)
                                                    (fromMaybe datum mdatum)
                                                    rscript)
    scriptData' = case mdatum of
      Nothing -> scriptData
      Just d -> case d of
        TxOutDatumNone       -> scriptData
        TxOutDatumHash{}     -> scriptData
        TxOutDatumInTx _ d   -> addDatum (toAlonzoData d) scriptData
        TxOutDatumInline _ d -> addDatum (toAlonzoData d) scriptData


applyTxMod tx utxos (ChangeInput txIn maddr mvalue mdatum) =
    (Tx (ShelleyTxBody era body scripts scriptData' auxData validity) wits, utxos')
  where
    Tx (ShelleyTxBody era body scripts scriptData auxData validity) wits = tx
    (addr, value, utxoDatum, rscript) = case Map.lookup txIn $ unUTxO utxos of
      Just (TxOut (AddressInEra _ (toAddressAny -> addr)) (txOutValueToValue -> value) datum rscript) ->
        (addr, value, datum, rscript)
      Nothing -> error $ "Index " ++ show txIn ++ " doesn't exist."

    txOut = TxOut (anyAddressInShelleyBasedEra (fromMaybe addr maddr))
                  (TxOutValue MultiAssetInBabbageEra $ fromMaybe value mvalue)
                  (fromMaybe utxoDatum $ toCtxUTxODatum <$> mdatum)
                  rscript
    utxos' = UTxO . Map.insert txIn txOut . unUTxO $ utxos

    scriptData' = case mdatum of
      Nothing                     -> scriptData
      Just TxOutDatumNone         -> scriptData
      Just TxOutDatumHash{}       -> scriptData
      Just (TxOutDatumInTx _ d)   -> addDatum (toAlonzoData d) scriptData
      Just (TxOutDatumInline _ d) -> addDatum (toAlonzoData d) scriptData

applyTxMod tx utxos (ChangeScriptInput txIn mvalue mdatum mredeemer) =
    (Tx (ShelleyTxBody era body scripts scriptData' auxData validity) wits, utxos')
  where
    Tx (ShelleyTxBody era body@Ledger.TxBody{..} scripts scriptData auxData validity) wits = tx
    (addr, value, utxoDatum, rscript) = case Map.lookup txIn $ unUTxO utxos of
      Just (TxOut addr (txOutValueToValue -> value) utxoDatum rscript) ->
        (addr, value, utxoDatum, rscript)
      Nothing -> error $ "The index " ++ show txIn ++ " doesn't exist."

    (datum, (redeemer, exunits)) = case scriptData of
      TxBodyNoScriptData -> error "No script data available"
      TxBodyScriptData _ (Ledger.TxDats dats) (Ledger.Redeemers rdmrs) ->
        (fromJust $ Map.lookup utxoDatumHash dats,
         fromJust $ Map.lookup (Ledger.RdmrPtr Ledger.Spend idx) rdmrs)

    utxoDatumHash = case utxoDatum of
      TxOutDatumNone       -> error "No existing datum"
      TxOutDatumInline _ d -> coerce $ hashScriptData d
      TxOutDatumHash _ h   -> coerce h

    adatum = case mdatum of
      Just TxOutDatumNone         -> error "Bad test!"
      Just TxOutDatumHash{}       -> error "Bad test!"
      Just (TxOutDatumInTx _ d)   -> toAlonzoData d
      Just (TxOutDatumInline _ d) -> toAlonzoData d
      Nothing                     -> datum

    txOut = TxOut addr
                  (TxOutValue MultiAssetInBabbageEra $ fromMaybe value mvalue)
                  (fromMaybe utxoDatum $ toCtxUTxODatum <$> mdatum)
                  rscript

    utxos' = UTxO . Map.insert txIn txOut . unUTxO $ utxos

    idx = case Ledger.indexOf (toShelleyTxIn txIn) inputs of
      SJust idx -> idx
      _         -> error "The impossible happened!"

    scriptData' = addScriptData idx adatum
                                    (fromMaybe redeemer (toAlonzoData <$> mredeemer), exunits)
                                    scriptData

applyTxMod _ _ (ReplaceTx tx utxos) = (tx, utxos)

-- | Add a new output of any type (public key or script)
addOutput :: AddressAny -> Value -> Datum -> TxModifier
addOutput addr value datum = txMod $ AddOutput addr value datum

-- | Remove an output of any type.
removeOutput :: Output -> TxModifier
removeOutput output = txMod $ RemoveOutput $ outputIx output

-- | Add a new public key input.
addKeyInput :: AddressAny -> Value -> Datum -> TxModifier
addKeyInput addr value datum = txMod $ AddInput addr value datum

-- | Remove an input of any type.
removeInput :: Input -> TxModifier
removeInput inp = txMod $ RemoveInput $ inputTxIn inp

-- | Add a plutus script input.
addPlutusScriptInput :: PlutusScript PlutusScriptV2 -> Value -> Datum -> Redeemer -> TxModifier
addPlutusScriptInput script value datum redeemer = txMod $ AddPlutusScriptInput script value datum redeemer

-- | Add a simple script input.
addSimpleScriptInput :: SimpleScript SimpleScriptV2 -> Value -> TxModifier
addSimpleScriptInput script value = txMod $ AddSimpleScriptInput script value

-- | Change the redeemer of a script input.
changeRedeemerOf :: Input -> Redeemer -> TxModifier
changeRedeemerOf i r
  | isKeyAddressAny (addressOf i) = error "Cannot changeRedeemerOf public key input"
  | otherwise                     = txMod $ ChangeScriptInput (inputTxIn i) Nothing Nothing (Just r)

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

