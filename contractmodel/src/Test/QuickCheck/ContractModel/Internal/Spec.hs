module Test.QuickCheck.ContractModel.Internal.Spec where

import Control.Monad.State as State
import Control.Monad.Reader
import Control.Monad.Writer
import Test.QuickCheck.StateModel
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Cardano.Api
import Test.QuickCheck.ContractModel.Symbolics
import Test.QuickCheck.ContractModel.Internal.Common
import Control.Lens
import Data.Foldable

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

instance MonadState state (Spec state) where
  state f = Spec $ State.state $ \s -> case f (_contractState s) of
      (a, cs) -> (a, s { _contractState = cs })
  {-# INLINE state #-}

  get = Spec $ fmap _contractState State.get
  {-# INLINE get #-}

  put cs = Spec $ State.modify' $ \s -> s { _contractState = cs }
  {-# INLINE put #-}

-- | Lens for the contract-specific part of the model state.
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
balanceChange :: Ord (AddressInEra Era) => AddressInEra Era -> Getter (ModelState state) SymValue
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
