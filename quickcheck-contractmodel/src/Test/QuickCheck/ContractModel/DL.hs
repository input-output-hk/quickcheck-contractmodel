module Test.QuickCheck.ContractModel.DL where

import Control.Monad

import Data.Typeable

import Test.QuickCheck.ContractModel.Internal.Model
import Test.QuickCheck.ContractModel.Internal.Spec
import Test.QuickCheck.ContractModel.Internal.ChainIndex
import Test.QuickCheck.ContractModel.Internal.Symbolics
import Test.QuickCheck.DynamicLogic          qualified as DL
import Test.QuickCheck.StateModel            qualified as StateModel
import Test.QuickCheck

import Cardano.Api

-- $dynamicLogic
--
-- Test scenarios are described in the `DL` monad (based on dynamic logic) which lets you freely mix
-- random sequences of actions (`anyAction`, `anyActions_`, `anyActions`) with specific
-- actions (`action`). It also supports checking properties of the model state (`DL.assert`,
-- `assertModel`), and random generation (`DL.forAllQ`).
--
-- For instance, a unit test for a simple auction contract might look something like this:
--
-- @
--  unitTest :: `DL` AuctionState ()
--  unitTest = do
--      `action` $ Bid w1 100
--      `action` $ Bid w2 150
--      `action` $ Wait endSlot
--      `action` $ Collect
-- @
--
--  and could easily be extended with some randomly generated values
--
-- @
--  unitTest :: `DL` AuctionState ()
--  unitTest = do
--      bid <- `forAllQ` $ `chooseQ` (1, 100)
--      `action` $ Bid w1 bid
--      `action` $ Bid w2 (bid + 50)
--      `action` $ Wait endSlot
--      `action` $ Collect
-- @
--
-- More interesting scenarios can be constructed by mixing random and fixed sequences. The following
-- checks that you can always finish an auction after which point there are no funds locked by the
-- contract:
--
-- @
-- finishAuction :: `DL` AuctionState ()
-- finishAuction = do
--   `anyActions_`
--   `action` $ Wait endSlot
--   `action` $ Collect
--   `assertModel` "Funds are locked!" (`Ledger.Value.isZero` . `lockedValue`)
-- @
--
-- `DL` scenarios are turned into QuickCheck properties using `forAllDL`.

-- $dynamicLogic_errors
--
-- In addition to failing the check that the emulator run matches the model, there are a few other
-- ways that test scenarios can fail:
--
-- * an explicit `action` does not satisfy its `precondition`
-- * a failed `DL.assert` or `assertModel`, or a monad `fail`
-- * an `Control.Applicative.empty` set of `Control.Applicative.Alternative`s
-- * the scenario fails to terminate (see `stopping`)
--
-- All of these occur at test case generation time, and thus do not directly say anything about the
-- contract implementation. However, together with the check that the model agrees with the emulator
-- they indirectly imply properties of the implementation. An advantage of this is that `DL` test
-- scenarios can be checked without running the contract through the emulator, which is much much
-- faster. For instance,
--
-- @
-- prop_FinishModel = `forAllDL` finishAuction $ const True
-- @
--
-- would check that the model does not think there will be any locked funds after the auction is
-- finished. Once this property passes, one can run the slower property that also checks that the
-- emulator agrees.

-- | The monad for writing test scenarios. It supports non-deterministic choice through
--   `Control.Applicative.Alternative`, failure with `MonadFail`, and access to the model state
--   through `GetModelState`. It is lazy, so scenarios can be potentially infinite, although the
--   probability of termination needs to be high enough that concrete test cases are always finite.
--   See `stopping` for more information on termination.
type DL state = DL.DL (ModelState state)

-- This is a trick to let you write `action $ WaitUntil n` in your DL test.
-- It helps you get around the fact that the DL test is printed in the qc-d world
-- where actions are `StateModel.Action`s not `ContractModel.Action`s.
-- So a DL counterexample will print like
-- ```
-- do action $ Offer
--    action $ Bid 1 10
--    action $ WaitUntil 10
--    ...
-- ```
-- Which wouldn't be copy-pastable unless we had this trick.
class ActionLike state a | a -> state where
  -- | Generate a specific action. Fails if the action's `precondition` is not satisfied.
  action :: a -> DL state ()

instance ContractModel state => ActionLike state (Action state) where
  action cmd = do
    s <- getModelState
    void $ DL.action (contractAction s cmd)

instance (ContractModel state, Typeable a) => ActionLike state (StateModel.Action (ModelState state) a) where
  action cmd = void $ DL.action cmd

observe :: ContractModel state => String -> ((forall t. HasSymbolicRep t => Symbolic t -> t) -> ChainState -> Bool) -> DL state ()
observe o p = action $ Observation o p

waitUntilDL :: forall state. ContractModel state => SlotNo -> DL state ()
waitUntilDL = action . WaitUntil

-- | Generate a random action using `arbitraryAction`. The generated action is guaranteed to satisfy
--   its `precondition`. Fails with `Stuck` if no action satisfying the precondition can be found
--   after 100 attempts.
anyAction :: DL state ()
anyAction = DL.anyAction

-- | Generate a sequence of random actions using `arbitraryAction`. All actions satisfy their
--   `precondition`s. The argument is the expected number of actions in the sequence chosen from a
--   geometric distribution, unless in the `stopping` stage, in which case as few actions as
--   possible are generated.
anyActions :: Int -> DL state ()
anyActions = DL.anyActions

-- | Generate a sequence of random actions using `arbitraryAction`. All actions satisfy their
--   `precondition`s. Actions may be generated until the `stopping` stage is reached; the expected length is size/2.
anyActions_ :: DL state ()
anyActions_ = DL.anyActions_

-- | Test case generation from `DL` scenarios have a target length of the action sequence to be
--   generated that is based on the QuickCheck size parameter (see `sized`). However, given that
--   scenarios can contain explicit `action`s it might not be possible to stop the scenario once the
--   target length has been reached.
--
--   Instead, once the target number of actions have been reached, generation goes into the
--   /stopping/ phase. In this phase branches starting with `stopping` are preferred, if possible.
--   Conversely, before the stopping phase, branches starting with `stopping`
--   are avoided unless there are no other possible choices.
--
--   For example, here is the definition of `anyActions`:
--
-- @
-- `anyActions` n = `stopping` `Control.Applicative.<|>` pure ()
--                        `Control.Applicative.<|>` (`weight` (fromIntegral n) >> `anyAction` >> `anyActions` n)
-- @
--
--   The effect of this definition is that the second or third branch will be taken until the desired number
--   of actions have been generated, at which point the `stopping` branch will be taken and
--   generation stops (or continues with whatever comes after the `anyActions` call).
--
--   Now, it might not be possible, or too hard, to find a way to terminate a scenario. For
--   instance, this scenario has no finite test cases:
--
-- @
-- looping = `anyAction` >> looping
-- @
--
--   To prevent test case generation from looping, if a scenario has not terminated after generating
--   @2 * n + 20@ actions, where @n@ is when the stopping phase kicks in, generation fails with a
--   `Looping` error.
stopping :: DL state ()
stopping = DL.stopping

-- | By default, `Control.Applicative.Alternative` choice (`Control.Applicative.<|>`) picks among
--   the next actions with equal probability. So, for instance, this code chooses between the actions
--   @a@, @b@ and @c@, with a probability @1/3@ of choosing each:
--
-- @
-- unbiasedChoice a b c = `action` a `Control.Applicative.<|>` `action` b `Control.Applicative.<|>` `action` c
-- @
--
--   To change this you can use `weight`, which multiplies the
--   relative probability of picking a branch by the given number.
--
--   For instance, the following scenario picks the action @a@ with probability @2/3@ and the action
--   @b@ with probability @1/3@:
--
-- @
-- biasedChoice a b = `weight` 2 (`action` a) `Control.Applicative.<|>` `weight` (`action` b)
-- @
--
--   Calls to `weight` need to appear at the top-level after a choice, preceding any actions
--   (`action`/`anyAction`) or random generation (`forAllQ`), or they will have no effect.
weight :: Double -> DL state ()
weight = DL.weight

-- | Sometimes test case generation should depend on QuickCheck's size
--   parameter. This can be accessed using @getSize@. For example, @anyActions_@ is defined by
--
-- @
-- anyActions_ = do n <- getSize
--                  anyActions (n `div` 2 + 1)
-- @
--
-- so that we generate a random number of actions, but on average half the size (which is about the same as
-- the average random positive integer, or length of a list).

getSize :: DL state Int
getSize = DL.getSize

-- | The `monitor` function allows you to collect statistics of your testing using QuickCheck
--   functions like `Test.QuickCheck.label`, `Test.QuickCheck.collect`, `Test.QuickCheck.classify`,
--   and `Test.QuickCheck.tabulate`. See also the `monitoring` method of `ContractModel` which is
--   called for all actions in a test case (regardless of whether they are generated by an explicit
--   `action` or an `anyAction`).
monitor :: (Property -> Property) -> DL state ()
monitor = DL.monitorDL

-- | Fail unless the given predicate holds of the model state.
--
--   Equivalent to
--
-- @
-- assertModel msg p = do
--   s <- `getModelState`
--   `DL.assert` msg (p s)
-- @
assertModel :: String -> (ModelState state -> Bool) -> DL state ()
assertModel = DL.assertModel

-- | Turn a `DL` scenario into a QuickCheck property. Generates a random `Actions` matching the
--   scenario and feeds it to the given property. The property can be a full property running the
--   emulator and checking the results, defined using `propRunActions_`, `propRunActions`, or
--   `propRunActionsWithOptions`. Assuming a model for an auction contract and `DL` scenario that
--   checks that you can always complete the auction, you can write:
--
-- @
-- finishAuction :: `DL` AuctionState ()
-- prop_Auction  = `propRunActions_` handles
--   where handles = ...
-- prop_Finish = `forAllDL` finishAuction prop_Auction
-- @
--
--   However, there is also value in a property that does not run the emulator at all:
--
-- @
-- prop_FinishModel = `forAllDL` finishAuction $ const True
-- @
--
--   This will check all the assertions and other failure conditions of the `DL` scenario very
--   quickly. Once this property passes a large number of tests, you can run the full property
--   checking that the model agrees with reality.
forAllDL :: (ContractModel state, Testable p)
         => DL state ()
         -> (Actions state -> p)
         -> Property
forAllDL dl prop = DL.forAllMappedDL id id fromStateModelActions dl prop

forAllUniqueDL :: forall state p. (ContractModel state, Testable p)
               => StateModel.Annotated (ModelState state)
               -> DL state ()
               -> (Actions state -> p)
               -> Property
forAllUniqueDL state dl prop = DL.forAllUniqueDL state dl (prop . fromStateModelActions)

instance ContractModel s => DL.DynLogicModel (ModelState s) where
    restricted (ContractAction _ act) = restricted act
    restricted WaitUntil{}            = False
    restricted Observation{}          = True

instance GetModelState (DL state) where
    type StateType (DL state) = state
    getModelState = DL.getModelStateDL
