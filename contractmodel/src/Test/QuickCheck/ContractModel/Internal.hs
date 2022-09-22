{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Test.QuickCheck.ContractModel.Internal where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.Writer as Writer
import Control.Monad.State as State

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.StateModel qualified as StateModel
import Test.QuickCheck.ContractModel.Symbolics
import Test.QuickCheck.ContractModel.Internal.Spec
import Test.QuickCheck.ContractModel.Internal.ChainIndex
import Test.QuickCheck.ContractModel.Internal.Model
import Test.QuickCheck.ContractModel.Internal.Common
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Data
import Data.Maybe
import Data.Generics.Uniplate.Data (universeBi)
import Data.Map (Map)
import Data.Map qualified as Map

import Cardano.Api

class (Monad m, HasChainIndex m, ContractModel state) => RunModel state m where
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
          -> RunMonad m (Map String AssetId)

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

newtype RunMonad m a = RunMonad { unRunMonad :: m a }
  deriving (Functor, Applicative, Monad)

instance ( Monad (RunMonad m)
         , RunModel state m
         , StateModel.Realized (RunMonad m) (Map String AssetId) ~ Map String AssetId
         ) => StateModel.RunModel (ModelState state) (RunMonad m) where
  perform _ _ _ = _

  monitoring (s0, s1) (ContractAction _ cmd) env res = monitoring @_ @m (s0, s1) cmd lookup res
    where lookup token = case Map.lookup (symVarIdx token) (env (symVar token)) of
                            Nothing  -> error $ "Unbound token: " ++ show token
                            Just aid -> aid
  monitoring (s0, _) (WaitUntil n@(SlotNo _n)) _ _ =
    tabulate "Wait interval" (bucket 10 diff) .
    tabulate "Wait until" (bucket 10 _n)
    where SlotNo diff = n - s0 ^. currentSlot

-- evaluteContractModel :: ( ContractModel state
--                         , RunModel state m
--                         )
--                      => Actions state
--                      -> PropertyM m (ModelState state, Map SymToken AssetId, ChainIndex) -- TODO: some datatype here?
-- evaluteContractModel as = do
--   ci <- run getChainIndex
--   (st, env) <- StateModel.runActions $ toStateModelActions as
--   ci' <- run getChainIndex
--   return (st, env, ci <> ci')

-- TODO: assert that chain index results match model state results?
-- * Here we need to deal with the issues around min ada etc.
-- * Here we might want flexibility - given that we now have postconditions
-- TODO: ContractModel.RunModel class
-- TODO: DL stuff?
