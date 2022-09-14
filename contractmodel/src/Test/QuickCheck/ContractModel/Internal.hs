{-# LANGUAGE UndecidableInstances #-}
module Test.QuickCheck.ContractModel.Internal where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State

import Test.QuickCheck
import Test.QuickCheck.StateModel qualified as SM
import Test.QuickCheck.StateModel hiding (Action)
import Test.QuickCheck.ContractModel.Symbolics
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Ord
import Data.List
import Data.Data
import Data.Foldable
import Data.Generics.Uniplate.Data (universeBi)

import Cardano.Api

type Era = BabbageEra

data ChainState = ChainState
  { slot :: SlotNo
  , utxo :: UTxO Era
  }

data TxInState = TxInState
  { tx         :: Tx Era
  , chainState :: ChainState
  }

data ChainIndex = ChainIndex
  { before       :: ChainState
  , after        :: ChainState
  , transactions :: [TxInState]
  }

instance Semigroup ChainIndex where
  ci <> ci' = ChainIndex { before       = minimumBy (comparing slot) [before ci, before ci']
                         , after        = maximumBy (comparing slot) [after ci, after ci']
                         , transactions = sortBy (comparing (slot . chainState))
                                        $ transactions ci ++ transactions ci'
                         }

class ContractTestable m where
  withChainIndex :: m a -> m (a, ChainIndex)

-- | The `ModelState` models the state of the blockchain. It contains,
--
--   * the contract-specific state (`contractState`)
--   * the current slot (`currentSlot`)
--   * the wallet balances (`balances`)
--   * the amount that has been minted (`minted`)
data ModelState state = ModelState
        { _currentSlot    :: SlotNo
        , _balanceChanges :: Map (AddressInEra Era) SymValue
        , _minted         :: SymValue
        , _symTokens      :: Set SymToken
        , _assertions     :: [(String, Bool)]
        , _assertionsOk   :: Bool
        , _contractState  :: state
        }
  deriving (Show)

instance Functor ModelState where
  fmap f m = m { _contractState = f (_contractState m) }

dummyModelState :: state -> ModelState state
dummyModelState s = ModelState 1 Map.empty mempty mempty mempty True s

-- | The `Spec` monad is a state monad over the `ModelState` with reader and writer components to keep track
--   of newly created symbolic tokens. It is used exclusively by the `nextState` function to model the effects
--   of an action on the blockchain.
newtype Spec state a = Spec { unSpec :: WriterT [SymToken] (ReaderT (Var AssetKey) (State (ModelState state))) a }
    deriving (Functor, Applicative, Monad)

-- | Lens for the contract-specific part of the model state.
--
--   `Spec` monad update functions: `$=` and `$~`.
makeLensesFor [("_contractState",  "contractState")]   'ModelState
makeLensesFor [("_currentSlot",    "currentSlotL")]    'ModelState
makeLensesFor [("_lastSlot",       "lastSlotL")]       'ModelState
makeLensesFor [("_balanceChanges", "balanceChangesL")] 'ModelState
makeLensesFor [("_minted",         "mintedL")]         'ModelState
makeLensesFor [("_tokenNameIndex", "tokenNameIndex")]  'ModelState
makeLensesFor [("_assertions", "assertions")]          'ModelState
makeLensesFor [("_assertionsOk", "assertionsOk")]      'ModelState
makeLensesFor [("_symTokens", "symTokens")]            'ModelState

-- | Get the current slot.
--
--   `Spec` monad update functions: `wait` and `waitUntil`.
currentSlot :: Getter (ModelState state) SlotNo
currentSlot = currentSlotL

-- | Get the current wallet balance changes. These are delta balances, so they start out at zero and
--   can be negative. The absolute balances used by the emulator can be set in the `CheckOptions`
--   argument to `propRunActionsWithOptions`.
--
--   `Spec` monad update functions: `withdraw`, `deposit`, `transfer`.
balanceChanges :: Getter (ModelState state) (Map (AddressInEra Era) SymValue)
balanceChanges = balanceChangesL

-- | Get the current balance change for a wallet. This is the delta balance, so it starts out at zero and
--   can be negative. The absolute balance used by the emulator can be set in the `CheckOptions`
--   argument to `propRunActionsWithOptions`.
--
--   `Spec` monad update functions: `withdraw`, `deposit`, `transfer`.
balanceChange :: AddressInEra Era -> Getter (ModelState state) SymValue
balanceChange w = balanceChangesL . at w . non mempty

-- | Get the amount of tokens minted so far. This is used to compute `lockedValue`.
--
--   `Spec` monad update functions: `mint` and `burn`.
minted :: Getter (ModelState state) SymValue
minted = mintedL

-- | How much value is currently locked by contracts. This computed by subtracting the wallet
--   `balances` from the `minted` value.
lockedValue :: ModelState s -> SymValue
lockedValue s = s ^. minted <> inv (fold $ s ^. balanceChanges)

class (Eq (Action state), Show (Action state)) => HasActions state where
  getAllSymtokens :: Action state -> Set SymToken

instance {-# OVERLAPPABLE #-} (Eq (Action state), Show (Action state), Data (Action state)) => HasActions state where
  getAllSymtokens = Set.fromList . universeBi

-- | A `ContractModel` instance captures everything that is needed to generate and run tests of a
--   contract or set of contracts. It specifies among other things
--
--  * what operations are supported by the contract (`Action`),
--  * when they are valid (`precondition`),
--  * how to generate random actions (`arbitraryAction`),
--  * how the operations affect the state (`nextState`), and
--  * how to run the operations in the emulator (`perform`)
class ( Typeable state
      , Show state
      , HasActions state
      ) => ContractModel state where

    -- | The type of actions that are supported by the contract. An action usually represents a single
    --   `Plutus.Trace.Emulator.callEndpoint` or a transfer of tokens, but it can be anything
    --   that can be interpreted in the `EmulatorTrace` monad.
    data Action state

    -- | Given the current model state, provide a QuickCheck generator for a random next action.
    --   This is used in the `Arbitrary` instance for `Actions`s as well as by `anyAction` and
    --   `anyActions`.
    arbitraryAction :: ModelState state -> Gen (Action state)

    -- | The name of an Action, used to report statistics.
    actionName :: Action state -> String
    actionName = head . words . show

    -- | The probability that we will generate a `WaitUntil` in a given state
    waitProbability :: ModelState state -> Double
    waitProbability _ = 0.1

    -- | Control the distribution of how long `WaitUntil` waits
    arbitraryWaitInterval :: ModelState state -> Gen SlotNo
    arbitraryWaitInterval s = SlotNo <$> choose (1, max 10 (head [ 5*(k-1) | k <- [0..], 2^k > n]))
      where
        SlotNo n = _currentSlot s

    -- | The initial state, before any actions have been performed.
    initialState :: state

    -- | The `precondition` function decides if a given action is valid in a given state. Typically
    --   actions generated by `arbitraryAction` will satisfy the precondition, but if they don't
    --   they will be discarded and another action will be generated. More importantly, the
    --   preconditions are used when shrinking (see `shrinkAction`) to ensure that shrunk test cases
    --   still make sense.
    --
    --   If an explicit `action` in a `DL` scenario violates the precondition an error is raised.
    precondition :: ModelState state -> Action state -> Bool
    precondition _ _ = True

    -- | `nextReactiveState` is run every time the model `wait`s for a slot to be reached. This
    --   can be used to model reactive components of off-chain code.
    nextReactiveState :: SlotNo -> Spec state ()
    nextReactiveState _ = return ()

    -- | This is where the model logic is defined. Given an action, `nextState` specifies the
    --   effects running that action has on the model state. It runs in the `Spec` monad, which is a
    --   state monad over the `ModelState`.
    nextState :: Action state -> Spec state ()

    -- | When a test involving random sequences of actions fails, the framework tries to find a
    --   minimal failing test case by shrinking the original failure. Action sequences are shrunk by
    --   removing individual actions, or by replacing an action by one of the (simpler) actions
    --   returned by `shrinkAction`.
    --
    --   See `Test.QuickCheck.shrink` for more information on shrinking.
    shrinkAction :: ModelState state -> Action state -> [Action state]
    shrinkAction _ _ = []
