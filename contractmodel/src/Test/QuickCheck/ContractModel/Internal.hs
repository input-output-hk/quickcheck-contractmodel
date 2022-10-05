{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Test.QuickCheck.ContractModel.Internal where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.Writer

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.StateModel qualified as StateModel
import Test.QuickCheck.ContractModel.Internal.Symbolics
import Test.QuickCheck.ContractModel.Internal.Spec
import Test.QuickCheck.ContractModel.Internal.ChainIndex
import Test.QuickCheck.ContractModel.Internal.Model
import Test.QuickCheck.ContractModel.Internal.Common
import Data.Map (Map)
import Data.Map qualified as Map
import Data.List
import Data.Typeable

import Cardano.Api

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
  deriving (Functor, Applicative, Monad, MonadWriter [(String, AssetId)])

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

class ( StateModel.Realized m (Map String AssetId) ~ Map String AssetId
      , StateModel.Realized m () ~ ()
      , HasChainIndex m
      , Monad m
      ) => IsRunnable m where
  waitUntil :: SlotNo -> m ()

instance (Monad m, HasChainIndex m) => HasChainIndex (RunMonad m) where
  getChainIndex = lift getChainIndex

instance IsRunnable m => IsRunnable (RunMonad m) where
  waitUntil = lift . waitUntil

instance ( IsRunnable m
         , RunModel state m
         ) => StateModel.RunModel (ModelState state) (RunMonad m) where
  perform st (ContractAction _ a) lookup = do
      -- Takes a `SymToken` and turns it into an `AssetId`
      let translate token = case Map.lookup (symVarIdx token) (lookup $ symVar token) of
            Just assetId -> assetId
            Nothing      -> error $ "The impossible happend: uncaught missing registerToken call for token: " ++ show token
      -- Run locally and get the registered tokens out
      tokens <- withLocalTokens $ perform st a translate
      -- Ask the model what tokens we expected to be registered in this run.
      -- NOTE: using `StateModel.Var 0` here is safe because we know that `StateModel` never uses `0` and we
      -- therefore get something unique. Likewise, we know that `nextState` can't observe the
      -- variables we use so it won't know the difference between having the real sym token
      -- it will get when we run `stateAfter` and this fake one.
      let expectedTokens = map symVarIdx $ tokensRegisterdBy (nextState a) (StateModel.Var 0) st
      -- If we the `createToken` and `registerToken` tokens don't correspond we have an issue!
      when (sort (Map.keys tokens) /= sort expectedTokens) $
        fail $ "Expected tokens: [" ++ intercalate "," expectedTokens ++ "] got [" ++ intercalate "," (Map.keys tokens) ++ "]"
      pure tokens
  perform _ (WaitUntil slot) _ = waitUntil slot

  monitoring (s0, s1) (ContractAction _ cmd) env res = monitoring @_ @m (s0, s1) cmd lookup res
    where lookup token = case Map.lookup (symVarIdx token) (env (symVar token)) of
                            Nothing  -> error $ "Unbound token: " ++ show token
                            Just aid -> aid
  monitoring (s0, _) (WaitUntil n@(SlotNo _n)) _ _ =
    tabulate "Wait interval" (bucket 10 diff) .
    tabulate "Wait until" (bucket 10 _n)
    where SlotNo diff = n - s0 ^. currentSlot

data ContractModelResult state = ContractModelResult
  { finalModelState :: ModelState state
  , symbolicTokens  :: Map SymToken AssetId
  , finalChainIndex :: ChainIndex
  }

runContractModel :: (ContractModel state, RunModel state m)
                 => Actions state
                 -> PropertyM (RunMonad m) (ContractModelResult state)
runContractModel as = do
  (st, env) <- StateModel.runActions $ toStateModelActions as
  ci        <- run getChainIndex
  return $ ContractModelResult { finalModelState = st
                               -- TODO: update this code to use `:=?` when that is merged to qc-d
                               , symbolicTokens = Map.fromList $ [ (SymToken v s, ai)
                                                                 | v StateModel.:=? m <- env
                                                                 , (s, ai) <- Map.toList m
                                                                 ]
                               , finalChainIndex = ci
                               }

-- TODO: assert that chain index results match model state results?
-- * Here we need to deal with the issues around min ada etc.
-- * Here we might want flexibility - given that we now have postconditions
-- TODO: DL stuff?
