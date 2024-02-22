{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -Wno-unused-matches -Wno-name-shadowing #-}
-- | The threat modelling framework allows you to write down and test properties of modifications of
--   valid transactions.
--
--   A threat model is represented by a value in the `ThreatModel` monad, and is evaluated in the
--   context of a single valid transaction and the chain state at the point it validated (a
--   `ThreatModelEnv`). Transactions and chain states can most easily be obtained using a
--   `ContractModelResult` from `runContractModel`, but they can in principle come from anywhere.
--
--   As an example, here is a `ThreatModel` that checks that any interaction with @myScript@
--   requires @theToken@ to be present:
--
-- @
--     tokenThreatModel :: 'ThreatModel' ()
--     tokenThreatModel = do
--       'ensureHasInputAt' myScript
--
--       let hasToken out = theToken ``leqValue`` 'valueOf' out
--       i <- 'anyInputSuchThat'  hasToken
--       o <- 'anyOutputSuchThat' hasToken
--
--       'shouldNotValidate' $ 'changeValueOf' i ('valueOf' i <> negateValue theToken)
--                        <> 'changeValueOf' o ('valueOf' o <> negateValue theToken)
-- @
--
--   For a more complex example see "Test.QuickCheck.ThreatModel.DoubleSatisfaction".
module Test.QuickCheck.ThreatModel
  ( -- * Transaction modifiers
    -- ** Types
    TxModifier
  , Input(..)
  , Output(..)
  , Datum
  , Redeemer
    -- ** Modifiers
  , IsInputOrOutput(..)
  , addOutput
  , removeOutput
  , addKeyInput
  , addPlutusScriptInput
  , addSimpleScriptInput
  , addReferenceScriptInput
  , addKeyReferenceInput
  , addPlutusScriptReferenceInput
  , addSimpleScriptReferenceInput
  , removeInput
  , changeRedeemerOf
  , changeValidityRange
  , changeValidityLowerBound
  , changeValidityUpperBound
  , replaceTx
    -- * Threat models
  , ThreatModel
  , ThreatModelEnv(..)
  , runThreatModel
  , assertThreatModel
  -- ** Preconditions
  , threatPrecondition
  , inPrecondition
  , ensure
  , ensureHasInputAt
  -- ** Validation
  , shouldValidate
  , shouldNotValidate
  , ValidityReport(..)
  , validate
  -- ** Querying the environment
  , getThreatModelEnv
  , originalTx
  , getTxInputs
  , getTxReferenceInputs
  , getTxOutputs
  , getRedeemer
  -- ** Random generation
  , forAllTM
  , pickAny
  , anySigner
  , anyInput
  , anyReferenceInput
  , anyOutput
  , anyInputSuchThat
  , anyReferenceInputSuchThat
  , anyOutputSuchThat
  -- ** Monitoring
  , counterexampleTM
  , tabulateTM
  , collectTM
  , classifyTM
  , monitorThreatModel
  , monitorLocalThreatModel
    -- * Cardano API helpers
    -- $cardanoHelpers
    -- ** Value
  , projectAda
  , leqValue
    -- ** Addresses
  , keyAddressAny
  , scriptAddressAny
  , isKeyAddressAny
    -- ** Datums
  , txOutDatum
  , toScriptData
  , datumOfTxOut
    -- * Pretty printing
    -- $prettyPrinting
  , paragraph
  , prettyAddress
  , prettyValue
  , prettyDatum
  , prettyInput
  , prettyOutput
  , module X
  ) where

import Cardano.Api as X
import Cardano.Api.Shelley as X

import Control.Monad

import Data.Map qualified as Map

import Test.QuickCheck

import Text.PrettyPrint hiding ((<>))
import Text.Printf

import Test.QuickCheck.ThreatModel.Cardano.Api
import Test.QuickCheck.ThreatModel.Pretty
import Test.QuickCheck.ThreatModel.TxModifier

-- $cardanoHelpers
-- Some convenience functions making it easier to work with Cardano API.

-- $prettyPrinting
-- The framework already prints the original transaction and the results of validating modified
-- transactions in counterexamples. To include more information you can use `counterexampleTM` with
-- the functions below.

-- | The context in which a `ThreatModel` is executed. Contains a transaction, its UTxO set and the
--   protocol parameters. See `getThreatModelEnv` and `originalTx` to access this information in a
--   threat model.
data ThreatModelEnv = ThreatModelEnv
  { currentTx    :: Tx Era
  , currentUTxOs :: UTxO Era
  , pparams      :: LedgerProtocolParameters Era
  }

-- | The threat model monad is how you construct threat models. It works in the context of a given
--   transaction and the UTxO set at the point where the transaction was validated (see
--   `ThreatModelEnv`) and lets you construct properties about the validatity of modifications of
--   the original transaction.
data ThreatModel a where
  Validate     :: TxModifier
               -> (ValidityReport -> ThreatModel a)
               -> ThreatModel a

  Generate     :: Show a
               => Gen a
               -> (a -> [a])
               -> (a -> ThreatModel b)
               -> ThreatModel b

  GetCtx       :: (ThreatModelEnv -> ThreatModel a) -> ThreatModel a

  Skip         :: ThreatModel a

  InPrecondition :: (Bool -> ThreatModel a)
                 -> ThreatModel a

  Fail         :: String
               -> ThreatModel a

  Monitor      :: (Property -> Property)
               -> ThreatModel a
               -> ThreatModel a


  MonitorLocal :: (Property -> Property)
               -> ThreatModel a
               -> ThreatModel a

  Done         :: a
               -> ThreatModel a

instance Functor ThreatModel where
  fmap = liftM

instance Applicative ThreatModel where
  pure  = Done
  (<*>) = ap

instance Monad ThreatModel where
  Validate tx cont      >>= k = Validate tx (cont >=> k)
  Skip                  >>= _ = Skip
  InPrecondition cont   >>= k = InPrecondition (cont >=> k)
  Fail err              >>= _ = Fail err
  Generate gen shr cont >>= k = Generate gen shr (cont >=> k)
  GetCtx cont           >>= k = GetCtx (cont >=> k)
  Monitor m cont        >>= k = Monitor m (cont >>= k)
  MonitorLocal m cont   >>= k = MonitorLocal m (cont >>= k)
  Done a                >>= k = k a

instance MonadFail ThreatModel where
  fail = Fail

-- | Evaluate a `ThreatModel` on a list of transactions with their context. Fails the property if
--   the threat model fails on any of the transactions.
runThreatModel :: ThreatModel a -> [ThreatModelEnv] -> Property
runThreatModel = go False
  where go b model [] = b ==> property True
        go b model (env : envs) = interp (counterexample $ show info) model
          where
            info =
              vcat [ ""
                   , block "Original UTxO set"
                           [prettyUTxO $ restrictUTxO (currentTx env)
                                       $ currentUTxOs env]
                   , ""
                   , block "Original transaction" [prettyTx $ currentTx env]
                   , ""
                   ]
            interp mon = \ case
              Validate mods k    -> interp mon
                                  $ k
                                  $ uncurry (validateTx $ pparams env)
                                  $ applyTxModifier (currentTx env) (currentUTxOs env) mods
              Generate gen shr k -> forAllShrinkBlind gen shr
                                  $ interp mon . k
              GetCtx k           -> interp mon
                                  $ k env
              Skip               -> go b model envs
              InPrecondition k   -> interp mon (k False)
              Fail err           -> mon $ counterexample err False
              Monitor m k        -> m $ interp mon k
              MonitorLocal m k   -> interp (mon . m) k
              Done{}             -> go True model envs

-- | Evaluate a `ThreatModel` on a list of transactions.
assertThreatModel :: ThreatModel a
                  -> LedgerProtocolParameters Era
                  -> [(Tx Era, UTxO Era)]
                  -> Property
assertThreatModel m params txs = runThreatModel m envs
  where
    envs = [ ThreatModelEnv tx utxo params
           | (tx, utxo) <- txs ]

-- | Check a precondition. If the argument threat model fails, the evaluation of the current
--   transaction is skipped. If all transactions in an evaluation of `runThreatModel` are skipped
--   it is considered a /discarded/ test for QuickCheck.
--
--   Having the argument to `threatPrecondition` be a threat model computation instead of a plain
--   boolean allows you do express preconditions talking about the validation of modified
--   transactions (using `shouldValidate` and `shouldNotValidate`). See `ensure` for the boolean
--   version.
threatPrecondition :: ThreatModel a -> ThreatModel a
threatPrecondition = \ case
  Skip             -> Skip
  InPrecondition k -> k True
  Fail reason      -> Monitor (tabulate "Precondition failed with reason" [reason]) Skip
  Validate tx k    -> Validate tx (threatPrecondition . k)
  Generate g s k   -> Generate g s (threatPrecondition . k)
  GetCtx k         -> GetCtx (threatPrecondition . k)
  Monitor m k      -> Monitor m (threatPrecondition k)
  MonitorLocal m k -> MonitorLocal m (threatPrecondition k)
  Done a           -> Done a

-- | Same as `threatPrecondition` but takes a boolean and skips the test if the argument is @False@.
ensure :: Bool -> ThreatModel ()
ensure False = Skip
ensure True  = pure ()

-- | Precondition that check that the original transaction has an input at a given address. Useful,
--   for example, to ensure that you only consider transactions that trie to spend a script output
--   from the script under test.
ensureHasInputAt :: AddressAny -> ThreatModel ()
ensureHasInputAt addr = do
  inputs <- getTxInputs
  ensure $ any ((addr ==) . addressOf) inputs

-- | Returns @True@ if evaluated under a `threatPrecondition` and @False@ otherwise.
inPrecondition :: ThreatModel Bool
inPrecondition = InPrecondition Done

-- | The most low-level way to validate a modified transaction. In most cases `shouldValidate` and
--   `shouldNotValidate` are preferred.
validate :: TxModifier -> ThreatModel ValidityReport
validate tx = Validate tx pure

-- | Check that a given modification of the original transaction validates. The modified transaction
--   is printed in counterexample when this fails, or if it succeeds in a precondition and the test
--   fails later.
shouldValidate :: TxModifier -> ThreatModel ()
shouldValidate = shouldValidateOrNot True

-- | Check that a given modification of the original transaction does not validate. The modified
--   transaction is printed in counterexample when it does validate, or if it doesn't in a satisfied
--   precondition and the test fails later.
shouldNotValidate :: TxModifier -> ThreatModel ()
shouldNotValidate = shouldValidateOrNot False

shouldValidateOrNot :: Bool -> TxModifier -> ThreatModel ()
shouldValidateOrNot should txMod = do
  validReport <- validate txMod
  ThreatModelEnv tx utxos _ <- getThreatModelEnv
  let newTx = fst $ applyTxModifier tx utxos txMod
      info str =
        block (text str)
          [ block "Modifications to original transaction"
                  [prettyTxModifier txMod]
          , block "Resulting transaction"
                  [prettyTx newTx]
          , text ""
          ]
      n't    | should    = "n't"
             | otherwise = "" :: String
      notN't | should    = "" :: String
             | otherwise = "n't"
  when (should /= valid validReport) $ do
    fail $ show $ info $ printf "Test failure: the following transaction did%s validate" n't
  pre <- inPrecondition
  when pre $
    counterexampleTM $ show $
      info $ printf "Satisfied precondition: the following transaction did%s validate" notN't

-- | Get the current context.
getThreatModelEnv :: ThreatModel ThreatModelEnv
getThreatModelEnv = GetCtx pure

-- | Get the original transaction from the context.
originalTx :: ThreatModel (Tx Era)
originalTx = currentTx <$> getThreatModelEnv

-- | Get the outputs from the original transaction.
getTxOutputs :: ThreatModel [Output]
getTxOutputs = zipWith (flip Output . TxIx) [0..] . txOutputs <$> originalTx

-- | Get the inputs from the original transaction.
getTxInputs :: ThreatModel [Input]
getTxInputs = do
  ThreatModelEnv tx (UTxO utxos) _ <- getThreatModelEnv
  pure [ Input txout i
       | i <- txInputs tx
       , Just txout <- [Map.lookup i utxos]
       ]

-- | Get the reference inputs from the original transaction.
getTxReferenceInputs :: ThreatModel [Input]
getTxReferenceInputs = do
  ThreatModelEnv tx (UTxO utxos) _ <- getThreatModelEnv
  pure [ Input txout i
       | i <- txReferenceInputs tx
       , Just txout <- [Map.lookup i utxos]
       ]

-- | Get the redeemer (if any) for an input of the original transaction.
getRedeemer :: Input -> ThreatModel (Maybe Redeemer)
getRedeemer (Input _ txIn) = do
  tx <- originalTx
  pure $ redeemerOfTxIn tx txIn

-- | Generate a random value. Takes a QuickCheck generator and a `shrink` function.
forAllTM :: Show a => Gen a -> (a -> [a]) -> ThreatModel a
forAllTM g s = Generate g s pure

-- | Pick a random input
anyInput :: ThreatModel Input
anyInput = anyInputSuchThat (const True)

-- | Pick a random reference input
anyReferenceInput :: ThreatModel Input
anyReferenceInput = anyReferenceInputSuchThat (const True)

-- | Pick a random output
anyOutput :: ThreatModel Output
anyOutput = anyOutputSuchThat (const True)

-- | Pick a random input satisfying the given predicate.
anyInputSuchThat :: (Input -> Bool) -> ThreatModel Input
anyInputSuchThat p = pickAny . filter p =<< getTxInputs

-- | Pick a random reference input satisfying the given predicate.
anyReferenceInputSuchThat :: (Input -> Bool) -> ThreatModel Input
anyReferenceInputSuchThat p = pickAny . filter p =<< getTxReferenceInputs

-- | Pick a random output satisfying the given predicate.
anyOutputSuchThat :: (Output -> Bool) -> ThreatModel Output
anyOutputSuchThat p = pickAny . filter p =<< getTxOutputs

-- | Pick a random value from a list. Skips the test if the list is empty.
pickAny :: Show a => [a] -> ThreatModel a
pickAny xs = do
  ensure (not $ null xs)
  let xs' = zip xs [0..]
  fst <$> forAllTM (elements xs') (\(_, i) -> take i xs')

-- | Pick a random signer of the original transaction.
anySigner :: ThreatModel (Hash PaymentKey)
anySigner = pickAny . txSigners =<< originalTx

-- | Monitoring that's shared between all transactions evaulated. Avoid this in favour of
--   `tabulateTM`, `collectTM` and `classifyTM` when possible.
monitorThreatModel :: (Property -> Property) -> ThreatModel ()
monitorThreatModel m = Monitor m (pure ())

-- | Monitoring that's local to the current transaction. Use `counterexampleTM` when possible.
monitorLocalThreatModel :: (Property -> Property) -> ThreatModel ()
monitorLocalThreatModel m = MonitorLocal m (pure ())

-- | Print the given string in case this threat model fails. Threat model counterpart of
--   the QuickCheck `Test.QuickCheck.counterexample` function.
counterexampleTM :: String -> ThreatModel ()
counterexampleTM = monitorLocalThreatModel . counterexample

-- | Threat model counterpart of QuickCheck's `Test.QuickCheck.tabulate` function.
tabulateTM :: String -> [String] -> ThreatModel ()
tabulateTM = (monitorThreatModel .) . tabulate

-- | Threat model counterpart of QuickCheck's `Test.QuickCheck.collect` function.
collectTM :: Show a => a -> ThreatModel ()
collectTM = monitorThreatModel . collect

-- | Threat model counterpart of QuickCheck's `Test.QuickCheck.classify` function.
classifyTM :: Bool -> String -> ThreatModel ()
classifyTM = (monitorThreatModel .) . classify

