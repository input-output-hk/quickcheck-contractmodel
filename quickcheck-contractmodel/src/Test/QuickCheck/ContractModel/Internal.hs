{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# OPTIONS_GHC -Wno-deprecations  #-}
module Test.QuickCheck.ContractModel.Internal where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Error

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.StateModel qualified as StateModel
import Test.QuickCheck.ContractModel.Internal.Symbolics
import Test.QuickCheck.ContractModel.Internal.Spec
import Test.QuickCheck.ContractModel.Internal.ChainIndex
import Test.QuickCheck.ContractModel.Internal.Model
import Test.QuickCheck.ContractModel.Internal.Utils
import Test.QuickCheck.ContractModel.Internal.Common
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Text.PrettyPrint.HughesPJClass hiding ((<>))

import Cardano.Api
import Cardano.Api.Shelley

class (ContractModel state, IsRunnable m) => RunModel state m where
  -- | Perform an `Action` in some `state` in the `Monad` `m`.  This
  -- is the function that's used to exercise the actual stateful
  -- implementation, usually through various side-effects as permitted
  -- by `m`. It produces a value of type `a`, eg. some observable
  -- output from the `Action` that should later be kept in the
  -- environment through a `Var a` also passed to the `nextState`
  -- function.
  --
  -- The `Lookup` parameter provides an /environment/ to lookup `Var
  -- a` instances from previous steps.
  perform :: ModelState state
          -> Action state
          -> (forall t. HasSymbolicRep t => Symbolic t -> t)
          -> RunMonad m ()

  -- | Allows the user to attach information to the `Property` at each step of the process.
  -- This function is given the full transition that's been executed, including the start and ending
  -- `state`, the `Action`, the current environment to `Lookup` and the value produced by `perform`
  -- while executing this step.
  monitoring :: (ModelState state, ModelState state)
             -> Action state
             -> (forall t. HasSymbolicRep t => Symbolic t -> t)
             -> SymIndex
             -> Property
             -> Property
  monitoring _ _ _ _ prop = prop

-- TODO: consider putting errors in this?
newtype RunMonad m a = RunMonad { unRunMonad :: WriterT SymIndex m a }
  deriving newtype (Functor, Applicative, Monad, MonadError e, MonadState s, MonadWriter SymIndex)

liftRunMonad :: (forall a. m a -> n a) -> RunMonad m a -> RunMonad n a
liftRunMonad f (RunMonad (WriterT m)) = RunMonad . WriterT $ f m

instance Monad m => MonadFail (RunMonad m) where
  fail = error

registerSymbolic :: (Monad m, HasSymbolicRep t)
                 => String
                 -> t
                 -> RunMonad m ()
registerSymbolic s = tell . symIndex s

registerToken :: Monad m => String -> AssetId -> RunMonad m ()
registerToken = registerSymbolic

registerTxOut :: Monad m => String -> TxOut CtxUTxO Era -> RunMonad m ()
registerTxOut = registerSymbolic

registerTxIn :: Monad m => String -> TxIn -> RunMonad m ()
registerTxIn = registerSymbolic

withLocalSymbolics :: Monad m => RunMonad m () -> RunMonad m SymIndex
withLocalSymbolics m = censor (const mempty) . fmap snd . listen $ m

instance MonadTrans RunMonad where
  lift = RunMonad . lift

type instance StateModel.Realized (RunMonad m) a = StateModel.Realized m a

type DefaultRealized m = ( StateModel.Realized m SymIndex ~ SymIndex
                         , StateModel.Realized m () ~ ()
                         )

class (DefaultRealized m, HasChainIndex m, Monad m) => IsRunnable m where
  awaitSlot :: SlotNo -> m ()

instance (Monoid w, DefaultRealized m, IsRunnable m) => IsRunnable (WriterT w m) where
  awaitSlot = lift . awaitSlot

instance (DefaultRealized m, IsRunnable m) => IsRunnable (StateT s m) where
  awaitSlot = lift . awaitSlot

instance (DefaultRealized m, IsRunnable m) => IsRunnable (ReaderT r m) where
  awaitSlot = lift . awaitSlot

instance (Monad m, HasChainIndex m) => HasChainIndex (RunMonad m) where
  getChainIndex = lift getChainIndex
  getChainState = lift getChainState

instance (Monad m, HasChainIndex m) => HasChainIndex (StateModel.PostconditionM m) where
  getChainIndex = lift getChainIndex
  getChainState = lift getChainState

instance IsRunnable m => IsRunnable (RunMonad m) where
  awaitSlot = lift . awaitSlot

-- Takes a `SymToken` and turns it into an `AssetId`
translateSymbolic :: (StateModel.Var SymIndex -> SymIndex) -> SymbolicSemantics
translateSymbolic lookup token = case lookupSymbolic (lookup $ symVar token) token of
  Just v  -> v
  Nothing -> error "The impossible happend: uncaught missing register for symbolic"

instance ( IsRunnable m
         , RunModel state m
         ) => StateModel.RunModel (ModelState state) (RunMonad m) where
  perform st (ContractAction _ _ a) lookup = do
      -- Run locally and get the registered symbolics out
      withLocalSymbolics $ perform st a (translateSymbolic lookup)
  perform _ (WaitUntil slot) _ = awaitSlot slot
  perform _ Observation{} _ = pure ()

  postcondition (st, _) (ContractAction StateModel.PosPolarity _ act) _ symIndex = do
    -- Ask the model what symbolics we expected to be registered in this run.
    -- NOTE: using `StateModel.Var 0` here is safe because we know that `StateModel` never uses `0` and we
    -- therefore get something unique. Likewise, we know that `nextState` can't observe the
    -- variables we use so it won't know the difference between having the real sym token
    -- it will get when we run `stateAfter` and this fake one.
    let expectedSymbolics = symbolicsCreatedBy (nextState act) (StateModel.mkVar 0) st
        actualSymbolics   = toCreationIndex symIndex
    StateModel.counterexamplePost $ unlines
      [ "Expected symbolics do not match registered symbolics"
      , "  Expected: " ++ show expectedSymbolics
      , "  Actual:   " ++ show actualSymbolics ]
    pure $ actualSymbolics == expectedSymbolics
  postcondition _ (Observation _ p) lookup _ = do
    cst <- getChainState
    pure $ p (translateSymbolic lookup) cst
  -- TODO: maybe add that current slot should equal the awaited slot?
  postcondition _ _ _ _ = pure True

  monitoring (s0, s1) (ContractAction _ _ act) env symIndex =
    monitoring @_ @m (s0, s1) act lookup symIndex
    where lookup :: HasSymbolicRep t => Symbolic t -> t
          lookup sym = case lookupSymbolic (env $ symVar sym) sym of
                            Nothing -> error $ "Unbound symbolic: " ++ show sym
                            Just v  -> v
  monitoring (s0, _) (WaitUntil n@(SlotNo _n)) _ _ =
    tabulate "Wait interval" (bucket 10 diff) .
    tabulate "Wait until" (bucket 10 _n)
    where SlotNo diff = n - s0 ^. currentSlot
  monitoring _ Observation{} _ _ = id

data ContractModelResult state = ContractModelResult
  { finalModelState :: ModelState state
  , symbolicTokens  :: Map SymToken AssetId
  , finalChainIndex :: ChainIndex
  }

runContractModel :: (ContractModel state, RunModel state m, HasChainIndex m)
                 => Actions state
                 -> PropertyM (RunMonad m) (ContractModelResult state)
runContractModel as = do
  (st, env) <- StateModel.runActions $ toStateModelActions as
  ci        <- run getChainIndex
  return $ ContractModelResult { finalModelState = StateModel.underlyingState st
                               -- Note, this is safe because we know what actions there
                               -- are at the StateModel level - only waits and underlying
                               -- actions that return new symtokens.
                               , symbolicTokens = Map.fromList $ [ (Symbolic v s, ai)
                                                                 | v StateModel.:=? m <- env
                                                                 , (s, ai) <- Map.toList $ m ^. tokens
                                                                 ]
                               , finalChainIndex = ci
                               }

data BalanceChangeOptions = BalanceChangeOptions
  { observeScriptValue   :: Bool
  , feeCalucation        :: FeeCalculation
  , protocolParameters   :: ProtocolParameters
  , addressPrettyPrinter :: AddressInEra Era -> String
  }

assertBalanceChangesMatch :: BalanceChangeOptions
                          -> ContractModelResult state
                          -> Property
assertBalanceChangesMatch (BalanceChangeOptions observeScript computeFees protoParams addressPrettyPrinter)
                          ContractModelResult{..} =
  let symbolicBalanceChanges  = _balanceChanges finalModelState
      predictedBalanceChanges = filterScripts $ fmap (toValue (fromJust . flip Map.lookup symbolicTokens)) symbolicBalanceChanges
      actualBalanceChanges    = filterScripts $ getBalanceChangesDiscountingFees finalChainIndex computeFees
      minAda                  = sum $ allMinAda finalChainIndex protoParams
      prettyChanges changes   = vcat
                                  [ sep [ text (addressPrettyPrinter addr) <> ":", nest 2 val ]
                                  | (addr, val) <- Map.toList changes ]
      msg = show $ vcat
             [ "Balance changes don't match:"
             , nest 2 $ vcat
               [ sep [ "Predicted symbolic balance changes:"
                     , nest 2 $ prettyChanges $ pPrint <$> symbolicBalanceChanges ]
               , sep [ "Predicted actual balance changes:"
                     , nest 2 $ prettyChanges $ pPrintValue <$> predictedBalanceChanges ]
               , sep [ "Actual balance changes:"
                     , nest 2 $ prettyChanges $ pPrintValue <$> actualBalanceChanges ]
               , "Sum of min Lovelace:" <+> text (show minAda)
               ]
             ]
      filterScripts m
        | observeScript = m
        | otherwise     = Map.filterWithKey (\ k _ -> not $ isScriptAddress k) m

      -- TODO: this is a hack because the module we need isn't exported. WTF?!
      isScriptAddress (AddressInEra (ShelleyAddressInEra _) addr) = isNothing $ shelleyPayAddrToPlutusPubKHash addr
      isScriptAddress _ = False

  in counterexample msg $ property $ checkEqualUpToMinAda minAda predictedBalanceChanges actualBalanceChanges
  where
    checkEqualUpToMinAda :: Lovelace
                         -> Map (AddressInEra Era) Value
                         -> Map (AddressInEra Era) Value
                         -> Bool
    checkEqualUpToMinAda l m m' =
      all (all isOk . valueToList) $ Map.unionWith (<>) m (negateValue <$> m')
      where
        isOk (AdaAssetId, q) = abs q <= lovelaceToQuantity l
        isOk (_, q)          = q == 0
