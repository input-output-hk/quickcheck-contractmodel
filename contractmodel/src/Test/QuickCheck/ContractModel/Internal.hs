{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
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
import Data.List
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
          -> (SymToken -> AssetId)
          -> RunMonad m ()

  -- | Allows the user to attach information to the `Property` at each step of the process.
  -- This function is given the full transition that's been executed, including the start and ending
  -- `state`, the `Action`, the current environment to `Lookup` and the value produced by `perform`
  -- while executing this step.
  monitoring :: (ModelState state, ModelState state)
             -> Action state
             -> (SymToken -> AssetId)
             -> Map String AssetId
             -> Property
             -> Property
  monitoring _ _ _ _ prop = prop

-- TODO: consider putting errors in this?
newtype RunMonad m a = RunMonad { unRunMonad :: WriterT [(String, AssetId)] m a }
  deriving (Functor, Applicative, Monad, MonadError e, MonadState s, MonadWriter [(String, AssetId)])

liftRunMonad :: (forall a. m a -> n a) -> RunMonad m a -> RunMonad n a
liftRunMonad f (RunMonad (WriterT m)) = RunMonad . WriterT $ f m

instance Monad m => MonadFail (RunMonad m) where
  fail = error

registerToken :: Monad m => String -> AssetId -> RunMonad m ()
registerToken s asset = tell [(s, asset)]

withLocalTokens :: Monad m => RunMonad m () -> RunMonad m (Map String AssetId)
withLocalTokens m = do
  tokens <- censor (const mempty) . fmap snd . listen $ m
  when (length tokens /= length (nub $ map fst tokens)) $
    fail $ "Duplicate call to registerToken: " ++ show tokens
  pure $ Map.fromList tokens

instance MonadTrans RunMonad where
  lift = RunMonad . lift

type instance StateModel.Realized (RunMonad m) a = StateModel.Realized m a
-- TODO: Because old version of qc-dynamic
type instance StateModel.Realized (WriterT w m) a = StateModel.Realized m a

type DefaultRealized m = ( StateModel.Realized m (Map String AssetId) ~ Map String AssetId
                         , StateModel.Realized m () ~ ()
                         )

class (DefaultRealized m, Monad m) => IsRunnable m where
  awaitSlot :: SlotNo -> m ()

instance (Monoid w, DefaultRealized m, IsRunnable m) => IsRunnable (WriterT w m) where
  awaitSlot = lift . awaitSlot

instance (DefaultRealized m, IsRunnable m) => IsRunnable (StateT s m) where
  awaitSlot = lift . awaitSlot

instance (DefaultRealized m, IsRunnable m) => IsRunnable (ReaderT r m) where
  awaitSlot = lift . awaitSlot

instance (Monad m, HasChainIndex m) => HasChainIndex (RunMonad m) where
  getChainIndex = lift getChainIndex

instance IsRunnable m => IsRunnable (RunMonad m) where
  awaitSlot = lift . awaitSlot

instance ( IsRunnable m
         , RunModel state m
         ) => StateModel.RunModel (ModelState state) (RunMonad m) where
  perform st (ContractAction _ a) lookup = do
      -- Takes a `SymToken` and turns it into an `AssetId`
      let translate token = case Map.lookup (symVarIdx token) (lookup $ symVar token) of
            Just assetId -> assetId
            Nothing      -> error $ "The impossible happend: uncaught missing registerToken call for token: " ++ show token
      -- Run locally and get the registered tokens out
      withLocalTokens $ perform st a translate
  perform _ (WaitUntil slot) _ = awaitSlot slot

  postcondition (st, _) (ContractAction _ act) _ tokens = do
    -- Ask the model what tokens we expected to be registered in this run.
    -- NOTE: using `StateModel.Var 0` here is safe because we know that `StateModel` never uses `0` and we
    -- therefore get something unique. Likewise, we know that `nextState` can't observe the
    -- variables we use so it won't know the difference between having the real sym token
    -- it will get when we run `stateAfter` and this fake one.
    let expectedTokens = map symVarIdx $ tokensCreatedBy (nextState act) (StateModel.Var 0) st
    -- If we the `createToken` and `registerToken` tokens don't correspond we have an issue!
    pure $ sort (Map.keys tokens) == sort expectedTokens
  -- TODO: maybe add that current slot should equal the awaited slot?
  postcondition _ _ _ _ = pure True

  -- TODO: a bit of code smell here because we don't have a nice solution to the
  -- counterexample in postcondition issue
  monitoring (s0, s1) (ContractAction _ act) env tokens =
      tokenCounterexample
    . monitoring @_ @m (s0, s1) act lookup tokens
    where lookup token = case Map.lookup (symVarIdx token) (env (symVar token)) of
                            Nothing  -> error $ "Unbound token: " ++ show token
                            Just aid -> aid
          expectedTokens = map symVarIdx $ tokensCreatedBy (nextState act) (StateModel.Var 0) s0
          tokenCounterexample
           | sort (Map.keys tokens) /= sort expectedTokens =
              counterexample ("Expected tokens: [" ++
                              intercalate "," expectedTokens ++
                              "] got [" ++
                              intercalate "," (Map.keys tokens) ++
                              "]")
           | otherwise = id
  monitoring (s0, _) (WaitUntil n@(SlotNo _n)) _ _ =
    tabulate "Wait interval" (bucket 10 diff) .
    tabulate "Wait until" (bucket 10 _n)
    where SlotNo diff = n - s0 ^. currentSlot

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
  return $ ContractModelResult { finalModelState = st
                               -- Note, this is safe because we know what actions there
                               -- are at the StateModel level - only waits and underlying
                               -- actions that return new symtokens.
                               , symbolicTokens = Map.fromList $ [ (SymToken v s, ai)
                                                                 | v StateModel.:=? m <- env
                                                                 , (s, ai) <- Map.toList m
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
