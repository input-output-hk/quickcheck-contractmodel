
module Test.QuickCheck.ThreatModel.Pretty where

import Cardano.Api
import Cardano.Api.Byron
import Cardano.Api.Shelley
import Cardano.Ledger.Alonzo.Tx qualified as Ledger (Data)
import Cardano.Ledger.Alonzo.TxWits qualified as Ledger
import Cardano.Ledger.SafeHash qualified as Ledger

import Cardano.Ledger.Alonzo.Scripts qualified as Ledger
import Data.ByteString qualified as BS
import Data.Char
import Data.List (nub, sort)
import Data.Map qualified as Map

import Text.PrettyPrint.HughesPJClass hiding ((<>))
import Text.Printf

import Test.QuickCheck.ThreatModel.TxModifier
import Test.QuickCheck.ThreatModel.Cardano.Api

-- | Format a list of strings as a paragraph. The structure of the list is not considered other than
--   inserting whitespace between consecutive elements. Use with
--   `Test.QuickCheck.ContractModel.ThreatModel.counterexampleTM`
--   when printing longer texts.
paragraph :: [String] -> String
paragraph s = show $ (fsep . map text . words . unwords $ s) $$ text ""

block :: Doc -> [Doc] -> Doc
block hd body = hang hd 2 $ vcat body

fblock :: Doc -> [Doc] -> Doc
fblock hd body = hang hd 2 $ fsep body

hblock :: Doc -> [Doc] -> Doc
hblock hd body = hd <+> fsep body

hblock' :: Int -> Doc -> [Doc] -> Doc
hblock' n hd body = hd $$ nest n (fsep body)

pList :: [Doc] -> Doc
pList = brackets . fsep . punctuate comma

pSet :: [Doc] -> Doc
pSet = braces . fsep . punctuate comma

pArgs :: [Doc] -> Doc
pArgs = parens . fsep . punctuate comma

infixr 6 <:>
(<:>) :: Doc -> Doc -> Doc
a <:> b = (a <> ":") <+> b

prettyInput :: Input -> Doc
prettyInput (Input txout txin) =
  prettyIn txin <:> prettyTxOut txout

prettyOutput :: Output -> Doc
prettyOutput (Output txout (TxIx i)) =
  brackets (text $ show i) <:> prettyTxOutTx txout

prettyUTxO :: UTxO Era -> Doc
prettyUTxO (UTxO utxos) =
  block "UTxOs" [ hblock' ind (prettyIn i <> ":") [prettyTxOut o] | (i, o) <- Map.toList utxos ]
  where
    ind | or [ i > 9 | TxIn _ (TxIx i) <- Map.keys utxos ] = 13
        | otherwise                                        = 12

prettyIn :: TxIn -> Doc
prettyIn (TxIn hash ix) =
  prettyHash hash <> brackets (prettyIx ix)

prettyTxOut :: TxOut CtxUTxO Era -> Doc
prettyTxOut (TxOut (AddressInEra _ addr) value datum _) = -- TODO: ref script
  hblock "TxOut" [ prettyAddress (toAddressAny addr)
                 , prettyValue (txOutValueToValue value)
                 , prettyDatum datum'
                 ]
  where
    datum' = case datum of
      TxOutDatumNone        -> TxOutDatumNone
      TxOutDatumHash s h    -> TxOutDatumHash s h
      TxOutDatumInline s sd -> TxOutDatumInline s sd


prettyTxOutTx :: TxOut CtxTx Era -> Doc
prettyTxOutTx (TxOut (AddressInEra _ addr) value datum _) = -- TODO: ref script
  hblock "TxOut" [ prettyAddress (toAddressAny addr)
                 , prettyValue (txOutValueToValue value)
                 , prettyDatum datum
                 ]

prettyAddress :: AddressAny -> Doc
prettyAddress (AddressByron (ByronAddress a)) = text $ show a
prettyAddress (AddressShelley (ShelleyAddress _ c _)) =
  case fromShelleyPaymentCredential c of
    PaymentCredentialByKey h    -> "Key#" <> prettyHash h
    PaymentCredentialByScript h -> "Script#" <> prettyHash h

prettyIx :: TxIx -> Doc
prettyIx (TxIx txIx) = text $ show txIx

prettyValue :: Value -> Doc
prettyValue value =
  pSet [ prettyAssetId assetId <:> text (show num)
       | (assetId, num) <- valueToList value ]

prettyAssetId :: AssetId -> Doc
prettyAssetId AdaAssetId = "lovelace"
prettyAssetId (AssetId hash name) = prettyHash hash <> "." <> prettyName name
  where
    prettyName (AssetName bs) = prettyBytes False bs

prettyHash :: Show a => a -> Doc
prettyHash = text . take 7 . drop 1 . show

prettyDatum :: Datum -> Doc
prettyDatum TxOutDatumNone         = empty
prettyDatum (TxOutDatumHash _ h)   = "Datum#" <> prettyHash h
prettyDatum (TxOutDatumInline _ d) = prettyScriptData $ getScriptData d
prettyDatum (TxOutDatumInTx _ d)   = prettyScriptData $ getScriptData d

prettyTx :: Tx Era -> Doc
prettyTx tx@(Tx body@(TxBody (TxBodyContent{..})) _) =
  block "Tx" [ "Valid:" <+> prettyValidity txValidityRange
             , fblock "Inputs:" $ map prettyIn inps
             , block "Outputs:" [ int i <:> prettyTxOutTx out
                                | (i, out) <- zip [0..] txOuts ]
             , prettyMinting txMintValue
             , prettyDatumMap scriptdat
             , block "Redeemers:" $ map (uncurry $ prettyRedeemer inps mnts) $ Map.toList rdmrs
             , block "Signed by:" $ map prettyHash (txSigners tx)
             ]
  where
    ShelleyTxBody _ _ _ scriptdat _ _ = body
    inps = sort . map fst $ txIns
    mnts = case txMintValue of
             TxMintNone          -> []
             TxMintValue _ val _ -> [ hash | AssetId hash _ <- sort . nub . map fst $ valueToList val ]
    rdmrs = case scriptdat of
              TxBodyScriptData _ _ (Ledger.Redeemers rdmrs) -> rdmrs
              TxBodyNoScriptData                            -> mempty

prettyRedeemer :: [TxIn] -> [PolicyId] -> Ledger.RdmrPtr -> (Ledger.Data era, Ledger.ExUnits) -> Doc
prettyRedeemer inps mints (Ledger.RdmrPtr tag ix) (dat, _) = pTag <:> prettyScriptData (getScriptData $ fromAlonzoData dat)
  where
    pTag =
      case tag of
        Ledger.Spend -> "Spend" <+> prettyIn (inps !! fromIntegral ix)
        Ledger.Mint  -> "Mint" <+> prettyHash (mints !! fromIntegral ix)
        _            -> text (show tag)

prettyDatumMap :: TxBodyScriptData Era -> Doc
prettyDatumMap (TxBodyScriptData _ (Ledger.TxDats dats) _)
  | not $ null dats =
    block "Datums:"
      [ prettyHash (Ledger.extractHash key) <:>
          prettyScriptData (getScriptData $ fromAlonzoData val)
      | (key, val) <- Map.toList dats
      ]
prettyDatumMap _ = empty

prettyMinting :: TxMintValue build Era -> Doc
prettyMinting TxMintNone            = empty
prettyMinting (TxMintValue _ val _) = block "Minting:" [prettyValue val]

prettyValidity :: (TxValidityLowerBound Era, TxValidityUpperBound Era) -> Doc
prettyValidity (TxValidityNoLowerBound, TxValidityNoUpperBound{}) = "any"
prettyValidity (lo, hi) = prettyLowerBound lo <+> "-" <+> prettyUpperBound hi

prettyLowerBound :: TxValidityLowerBound Era -> Doc
prettyLowerBound TxValidityNoLowerBound        = "-∞"
prettyLowerBound (TxValidityLowerBound _ slot) = text (show $ unSlotNo slot)

prettyUpperBound :: TxValidityUpperBound Era -> Doc
prettyUpperBound TxValidityNoUpperBound{}      = "∞"
prettyUpperBound (TxValidityUpperBound _ slot) = text (show $ unSlotNo slot)

prettyTxModifier :: TxModifier -> Doc
prettyTxModifier (TxModifier txmod) = vcat [prettyMod mod | mod <- txmod]
  where
    prettyPlutusScript = prettyHash . hashScript . PlutusScript PlutusScriptV2
    prettySimpleScript = prettyHash . hashScript . SimpleScript

    maybeBlock _ _ _ Nothing   = empty
    maybeBlock tag hd pr (Just d) = hang tag 2 $ fsep [hd, pr d]

    prettyMod (RemoveInput txIn) =
      "removeInput" <+> prettyIn txIn

    prettyMod (RemoveOutput ix) =
      "removeOutput" <+> prettyIx ix

    prettyMod (ChangeOutput ix maddr mvalue mdatum) =
      vcat [ maybeBlock "changeAddressOf" (prettyIx ix) prettyAddress maddr
           , maybeBlock "changeValueOf"   (prettyIx ix) prettyValue mvalue
           , maybeBlock "changeDatumOf"   (prettyIx ix) prettyDatum mdatum
           ]

    prettyMod (ChangeInput txIn maddr mvalue mdatum) =
      vcat [ maybeBlock "changeAddressOf" (prettyIn txIn) prettyAddress maddr
           , maybeBlock "changeValueOf"   (prettyIn txIn) prettyValue mvalue
           , maybeBlock "changeDatumOf"   (prettyIn txIn) prettyDatum mdatum
           ]

    prettyMod (ChangeScriptInput txIn mvalue mdatum mrdmr) =
      vcat [ maybeBlock "changeValueOf"    (prettyIn txIn) prettyValue mvalue
           , maybeBlock "changeDatumOf"    (prettyIn txIn) prettyDatum mdatum
           , maybeBlock "changeRedeemerOf" (prettyIn txIn) prettyScriptData mrdmr
           ]

    prettyMod (AddOutput addr value datum) =
      fblock "addOutput" [ prettyAddress addr
                         , prettyValue value
                         , prettyDatum datum
                         ]

    prettyMod (AddInput addr value datum) =
      fblock "addInput" [ prettyAddress addr
                        , prettyValue value
                        , prettyDatum datum
                        ]

    prettyMod (AddPlutusScriptInput script value datum redeemer) =
      fblock "addPlutusScriptInput" [ prettyPlutusScript script
                                    , prettyValue value
                                    , prettyDatum datum
                                    , prettyScriptData redeemer
                                    ]

    prettyMod (AddSimpleScriptInput script value) =
      fblock "addSimpleScriptInput" [ prettySimpleScript script
                                    , prettyValue value
                                    ]

    prettyMod (ChangeValidityRange (Just lo) (Just hi)) =
      fblock "changeValidityRange" [ prettyValidity (lo, hi) ]
    prettyMod (ChangeValidityRange mlo mhi) =
      vcat [ maybeBlock "changeValidityLowerBound" empty prettyLowerBound mlo
           , maybeBlock "changeValidityUpperBound" empty prettyUpperBound mhi
           ]

    prettyMod (ReplaceTx tx utxos) =
      fblock "replaceTx" [ prettyUTxO utxos
                         , prettyTx tx
                         ]

prettyScriptData :: ScriptData -> Doc
prettyScriptData (ScriptDataConstructor i args) = "Con" <> text (show i) <> pArgs (map prettyScriptData args)
prettyScriptData (ScriptDataMap map) = pSet
  [ prettyScriptData k <:> prettyScriptData v | (k, v) <- map ]
prettyScriptData (ScriptDataList list) = pList $ map prettyScriptData list
prettyScriptData (ScriptDataNumber n) = text (show n)
prettyScriptData (ScriptDataBytes bs) = prettyBytes True bs

prettyBytes :: Bool -> BS.ByteString -> Doc
prettyBytes quotes bs
  | any (not . isPrint) s = text $ take 7 $ concatMap (printf "%02x") $ map fromEnum s
  | quotes                = text (show bs)
  | otherwise             = text s
  where
    s = map (toEnum . fromIntegral) $ BS.unpack bs

