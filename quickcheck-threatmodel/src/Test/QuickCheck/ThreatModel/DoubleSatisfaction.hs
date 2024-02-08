
module Test.QuickCheck.ThreatModel.DoubleSatisfaction
  ( doubleSatisfaction
  ) where

import PlutusTx.Prelude (BuiltinByteString)

import Test.QuickCheck.ThreatModel

safeScript :: SimpleScript
safeScript = RequireAllOf [] -- TODO: this is not the right script!

-- | Check for double satisfaction vulnerabilities.
--
--   For a transaction with a public key output to an address (the victim) other than the signer
--   (the attacker),
--
--   * if you cannot redirect (the Ada from) the victim to the attacker, i.e. there is a script that
--     care about the output to the victim,
--   * but it validates when you bundle the redirected transaction with a "safe script" that spends
--     the same amount to the victim, tagging the output with a unique datum,
--
--   then we have found a double satisfaction vulnerability in the script that stopped the first
--   modified transaction.
doubleSatisfaction :: ThreatModel ()
doubleSatisfaction = do

  signer <- keyAddressAny <$> anySigner

  outputs <- getTxOutputs
  let validTarget t = signer /= t && isKeyAddressAny t
  output  <- pickAny $ filter (validTarget . addressOf) outputs

  let ada = projectAda $ valueOf output

  counterexampleTM $
    paragraph $
      [ "The transaction above is signed by"
      , show $ prettyAddress signer
      , "and contains an output to"
      , show (prettyAddress $ addressOf output) ++ "."
      , "The objective is to show that there is a double satisfaction vulnerability"
      , "that allows the signer to steal this output."
      ]

  counterexampleTM $
    paragraph [ "First we check that we cannot simply redirect the output to the signer,"
              , "i.e. the script actually cares about this output." ]

  threatPrecondition $ shouldNotValidate $ changeValueOf output (valueOf output <> negateValue ada)
                                        <> addOutput signer ada TxOutDatumNone ReferenceScriptNone

  counterexampleTM $
    paragraph [ "Now we try the same thing again, but this time there is another script"
              , "that pays out to the victim and uses a unique datum to identify the payment."
              ]

  -- add safe script input with protected output, redirect original output to signer
  let uniqueDatum = txOutDatum $ toScriptData ("SuchSecure" :: BuiltinByteString)

      victimTarget = addressOf output

  shouldNotValidate $ addSimpleScriptInput safeScript ada ReferenceScriptNone
                   <> addOutput      victimTarget ada uniqueDatum ReferenceScriptNone
                   <> changeValueOf  output (valueOf output <> negateValue ada)
                   <> addOutput      signer ada TxOutDatumNone ReferenceScriptNone

