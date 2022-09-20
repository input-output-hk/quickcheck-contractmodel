{-# LANGUAGE UndecidableInstances #-}
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
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Data
import Data.Maybe
import Data.Generics.Uniplate.Data (universeBi)

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
  perform :: forall a. Typeable a => ModelState state -> Action state a -> StateModel.LookUp m -> m (StateModel.Realized m a)

  -- | Postcondition on the `a` value produced at some step.
  -- The result is `assert`ed and will make the property fail should it be `False`. This is useful
  -- to check the implementation produces expected values.
  postcondition :: forall a. (ModelState state, ModelState state) -> Action state a -> StateModel.LookUp m -> StateModel.Realized m a -> m Bool
  postcondition _ _ _ _ = pure True

  -- | Allows the user to attach information to the `Property` at each step of the process.
  -- This function is given the full transition that's been executed, including the start and ending
  -- `state`, the `Action`, the current environment to `Lookup` and the value produced by `perform`
  -- while executing this step.
  monitoring :: forall a. (ModelState state, ModelState state) -> Action state a -> StateModel.LookUp m -> StateModel.Realized m a -> Property -> Property
  monitoring _ _ _ _ prop = prop

evaluteContractModel :: ( ContractModel state
                        , RunModel state m
                        )
                     => Actions state
                     -> PropertyM m (ModelState state, StateModel.Env m, ChainIndex) -- TODO: some datatype here?
evaluteContractModel as = do
  ci <- run getChainIndex
  (st, env) <- StateModel.runActions $ toStateModelActions as
  ci' <- run getChainIndex
  return (st, env, ci <> ci')

-- TODO: assert that chain index results match model state results?
-- * Here we need to deal with the issues around min ada etc.
-- * Here we might want flexibility - given that we now have postconditions
-- TODO: ContractModel.RunModel class
-- TODO: DL stuff?
