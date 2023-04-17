{-# LANGUAGE UndecidableInstances #-}
module Test.QuickCheck.ContractModel.Internal.Model
  ( HasSymTokens(..)
  , BaseType(..)
  , GenericHasSymTokens(..)
  , ContractModel(..)
  , createsTokens
  , wait
  , waitUntil
  , contractAction
  , Actions(..)
  , pattern ContractAction
  , pattern WaitUntil
  , pattern Observation
  , pattern Actions
  , Act(..)
  , mapActions
  , isBind
  , toStateModelActions
  , fromStateModelActions
  , dummyModelState
  , stateAfter
  , annotatedStateAfter
  , asserts
  ) where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.Writer as Writer
import Control.Monad.State as State

import Test.QuickCheck
import Test.QuickCheck.StateModel qualified as StateModel
import Test.QuickCheck.ContractModel.Internal.Symbolics
import Test.QuickCheck.ContractModel.Internal.Spec
import Test.QuickCheck.ContractModel.Internal.Common
import Test.QuickCheck.ContractModel.Internal.ChainIndex
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Data
import Data.Maybe
import Data.Map (Map)
import Data.Map qualified as Map
import GHC.Generics as Generic

import Cardano.Api

type HasActions state = ( Eq (Action state)
                        , Show (Action state)
                        -- TODO: here we are re-using a bunch of stuff related to
                        -- variables! That's not the right design!! I think we need
                        -- a fundamental re-design of the variable stuff...
                        , HasSymTokens (Action state)
                        , StateModel.HasVariables state
                        , StateModel.HasVariables (Action state)
                        )

class HasSymTokens a where
  getAllSymTokens :: a -> Set SymToken

  default getAllSymTokens :: (Generic a, GenericHasSymTokens (Rep a)) => a -> Set SymToken
  getAllSymTokens = genericGetAllSymTokens . Generic.from

newtype BaseType a = BaseType a

instance HasSymTokens SymToken where
  getAllSymTokens = Set.singleton

instance HasSymTokens (BaseType a) where
  getAllSymTokens _ = mempty

deriving via BaseType Integer  instance HasSymTokens Integer
deriving via BaseType Int      instance HasSymTokens Int
deriving via BaseType Char     instance HasSymTokens Char
deriving via BaseType Value    instance HasSymTokens Value
deriving via BaseType Quantity instance HasSymTokens Quantity
-- TODO: update to these once the whole HasNoVariables gadget has been exported in
-- the next release of quickcheck-dynamic
-- deriving via StateModel.HasNoVariables T instance StateModel.HasVariables T
instance StateModel.HasVariables (AddressInEra Era) where
  getAllVariables _ = mempty
instance StateModel.HasVariables Quantity where
  getAllVariables _ = mempty
instance StateModel.HasVariables Value where
  getAllVariables _ = mempty

instance (HasSymTokens k, HasSymTokens v) => HasSymTokens (Map k v) where
  getAllSymTokens = getAllSymTokens . Map.toList

instance {-# OVERLAPPABLE #-} (Generic a, GenericHasSymTokens (Rep a)) => HasSymTokens a

class GenericHasSymTokens f where
  genericGetAllSymTokens :: f k -> Set SymToken

instance GenericHasSymTokens f => GenericHasSymTokens (M1 i c f) where
  genericGetAllSymTokens = genericGetAllSymTokens . unM1

instance HasSymTokens c => GenericHasSymTokens (K1 i c) where
  genericGetAllSymTokens = getAllSymTokens . unK1

instance GenericHasSymTokens U1 where
  genericGetAllSymTokens _ = mempty

instance (GenericHasSymTokens f, GenericHasSymTokens g) => GenericHasSymTokens (f :*: g) where
  genericGetAllSymTokens (x :*: y) = genericGetAllSymTokens x <> genericGetAllSymTokens y

instance (GenericHasSymTokens f, GenericHasSymTokens g) => GenericHasSymTokens (f :+: g) where
  genericGetAllSymTokens (L1 x) = genericGetAllSymTokens x
  genericGetAllSymTokens (R1 x) = genericGetAllSymTokens x

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

    restricted :: Action state -> Bool
    restricted _ = False

-- | Check if a given action creates new symbolic tokens in a given `ModelState`
createsTokens :: ContractModel state
              => ModelState state
              -> Action state
              -> Bool
createsTokens s a = ([] /=) $ State.evalState (runReaderT (snd <$> Writer.runWriterT (unSpec (nextState a)))
                                                          (StateModel.mkVar 0)) s

-- | Wait the given number of slots. Updates the `currentSlot` of the model state.
wait :: ContractModel state => Integer -> Spec state ()
wait 0 = return ()
wait n = do
  now <- viewModelState currentSlot
  nextReactiveState (now + fromIntegral n)
  modState currentSlotL (const (now + fromIntegral n))

-- | Wait until the given slot. Has no effect if `currentSlot` is greater than the given slot.
waitUntil :: ContractModel state => SlotNo -> Spec state ()
waitUntil n = do
  now <- viewModelState currentSlot
  when (now < n) $ do
    let SlotNo n' = n - now
    wait (fromIntegral n')

instance ContractModel state => Show (StateModel.Action (ModelState state) a) where
    showsPrec p (ContractAction _ a) = showsPrec p a
    showsPrec p (WaitUntil n)        = showParen (p >= 11) $ showString "WaitUntil " . showsPrec 11 n
    showsPrec _ o@Observation{}      = showString $ StateModel.actionName o

instance ContractModel state => Eq (StateModel.Action (ModelState state) a) where
  ContractAction b a == ContractAction b' a' = (b, a) == (b', a')
  WaitUntil s == WaitUntil s'                = s == s'
  Observation o _ == Observation o' _        = o == o'
  _ == _                                     = False

contractAction :: ContractModel state
               => ModelState state
               -> Action state
               -> StateModel.Action (ModelState state) (Map String AssetId)
contractAction s a = ContractAction (createsTokens s a) a

instance StateModel.HasVariables (Action state) =>
          StateModel.HasVariables (StateModel.Action (ModelState state) a) where
  getAllVariables (ContractAction _ a) = StateModel.getAllVariables a
  getAllVariables WaitUntil{}          = mempty
  getAllVariables Observation{}        = mempty

instance ContractModel state => StateModel.StateModel (ModelState state) where
  data Action (ModelState state) a where
    ContractAction :: Bool
                   -> Action state
                   -> StateModel.Action (ModelState state) (Map String AssetId)
    Observation :: String
                -- Note: the `SymToken -> AssetId` argument is necessary because
                -- when the user calls `observe` in their DL property to issue one
                -- of these actions they are at _generation time_ so can't do `SymToken`
                -- resolution up-front, instead it has to happen in the `perform`
                -- for the `Observation` action.
                -> ((SymToken -> AssetId) -> ChainState -> Bool)
                -> StateModel.Action (ModelState state) ()
    WaitUntil :: SlotNo
              -> StateModel.Action (ModelState state) ()

  actionName (ContractAction _ act) = actionName act
  actionName WaitUntil{}            = "WaitUntil"
  actionName (Observation n _)      = "Observation[" ++ n ++ "]"

  arbitraryAction _ s =
    frequency [( floor $ 100.0*(1.0-waitProbability s)
               , do a <- arbitraryAction s
                    return (StateModel.Some (ContractAction (createsTokens s a) a)))
              ,( floor $ 100.0*waitProbability s
               , StateModel.Some . WaitUntil . step <$> arbitraryWaitInterval s)]
        where
            slot = s ^. currentSlot
            step n = slot + n

  shrinkAction _ s (ContractAction _ a) =
    [ StateModel.Some (WaitUntil (SlotNo n'))
    | let SlotNo n = runSpec (nextState a) (StateModel.mkVar 0) s ^. currentSlot
    , n' <- n : shrink n
    , SlotNo n' > s ^. currentSlot ] ++
    [ StateModel.Some (contractAction s a')
    | a' <- shrinkAction s a ]
  shrinkAction _ s (WaitUntil (SlotNo n)) =
    [ StateModel.Some (WaitUntil (SlotNo n'))
    | n' <- shrink n
    , SlotNo n' > s ^. currentSlot ]
  shrinkAction _ _ Observation{} = []

  initialState = ModelState { _currentSlot      = 1
                            , _balanceChanges   = mempty
                            , _minted           = mempty
                            , _assertions       = mempty
                            , _assertionsOk     = True
                            , _symTokens        = mempty
                            , _contractState    = initialState
                            }

  nextState s (ContractAction _ cmd) v = runSpec (nextState cmd) v s
  nextState s (WaitUntil n) _          = runSpec (() <$ waitUntil n) (error "unreachable") s
  nextState s (Observation _ _) _      = s

  -- Note that the order of the preconditions in this case matter - we want to run
  -- `getAllSymTokens` last because its likely to be stricter than the user precondition
  -- and so if the user relies on the lazyness of the Gen monad by using the precondition
  -- to avoid duplicate checks in the precondition and generator we don't screw that up.
  precondition s (ContractAction _ cmd) = s ^. assertionsOk
                                        && precondition s cmd
                                        && getAllSymTokens cmd `Set.isSubsetOf` (s ^. symTokens)
  precondition s (WaitUntil n)          = n > s ^. currentSlot
  precondition _ Observation{}          = True

-- We include a list of rejected action names.
data Actions s = Actions_ [String] (Smart [Act s])

{-# COMPLETE Actions #-}
pattern Actions :: [Act s] -> Actions s
pattern Actions as <- Actions_ _ (Smart _ as) where
  Actions as = Actions_ [] (Smart 0 as)

data Act s = Bind   {varOf :: StateModel.Var (Map String AssetId), actionOf :: Action s}
           | NoBind {varOf :: StateModel.Var (Map String AssetId), actionOf :: Action s}
           | ActWaitUntil (StateModel.Var ()) SlotNo
           | ActObservation (StateModel.Var ()) String ((SymToken -> AssetId) -> ChainState -> Bool)

mapActions :: (Action s -> Action s') -> Actions s -> Actions s'
mapActions f (Actions_ rej (Smart n as)) = Actions_ rej $ Smart n $ map mapAct as
  where
    mapAct (Bind x a)             = Bind x (f a)
    mapAct (NoBind x a)           = NoBind x (f a)
    mapAct (ActWaitUntil x n)     = ActWaitUntil x n
    mapAct (ActObservation x n p) = ActObservation x n p

instance ContractModel s => Eq (Act s) where
  Bind v a             == Bind v' a'             = (v, a) == (v', a')
  NoBind v a           == NoBind v' a'           = (v, a) == (v', a')
  ActWaitUntil v s     == ActWaitUntil v' s'     = (v, s) == (v', s')
  ActObservation v n _ == ActObservation v' n' _ = (v, n) == (v', n')
  _                    == _                      = False

isBind :: Act s -> Bool
isBind Bind{} = True
isBind _      = False

instance ContractModel state => Show (Act state) where
  showsPrec d (Bind v a) =
    showParen (d >= 11) $ showString ("tok." ++ show v ++ " := ") . showsPrec 0 a
  showsPrec d (ActWaitUntil _ n) = showsPrec d (WaitUntil @state n)
  showsPrec d (ActObservation _ n p) = showsPrec d (Observation @state n p)
  showsPrec d (NoBind _ a) =
    showsPrec d a

instance ContractModel state => Show (Actions state) where
  showsPrec d (Actions as)
    | d>10      = ("("++).showsPrec 0 (Actions as).(")"++)
    | null as   = ("Actions []"++)
    | otherwise = ("Actions \n [" ++) .
                  foldr (.) (showsPrec 0 (last as) . ("]"++))
                    [showsPrec 0 a . (",\n  "++) | a <- init as]

instance ContractModel s => Arbitrary (Actions s) where
  arbitrary = fromStateModelActions <$> arbitrary
  shrink = map fromStateModelActions . shrink . toStateModelActions

toStateModelActions :: ContractModel state =>
                        Actions state -> StateModel.Actions (ModelState state)
toStateModelActions (Actions_ rs (Smart k s)) =
  StateModel.Actions_ rs (Smart k $ map mkStep s)
    where mkStep (ActWaitUntil v n) = v StateModel.:= WaitUntil n
          mkStep (ActObservation v n p) = v StateModel.:= Observation n p
          mkStep act                = varOf act StateModel.:= ContractAction (isBind act) (actionOf act)

fromStateModelActions :: StateModel.Actions (ModelState s) -> Actions s
fromStateModelActions (StateModel.Actions_ rs (Smart k s)) =
  Actions_ rs (Smart k (catMaybes $ map mkAct s))
  where
    mkAct :: StateModel.Step (ModelState s) -> Maybe (Act s)
    mkAct (v StateModel.:= ContractAction b act)
          | b         = Just $ Bind   v act
          | otherwise = Just $ NoBind v act
    mkAct (v StateModel.:= WaitUntil n) = Just $ ActWaitUntil v n
    mkAct (v StateModel.:= Observation n p) = Just $ ActObservation v n p

dummyModelState :: state -> ModelState state
dummyModelState s = ModelState 1 Map.empty mempty mempty mempty True s

stateAfter :: ContractModel state => Actions state -> ModelState state
stateAfter = StateModel.underlyingState . StateModel.stateAfter . toStateModelActions

annotatedStateAfter :: ContractModel state => Actions state -> StateModel.Annotated (ModelState state)
annotatedStateAfter = StateModel.stateAfter . toStateModelActions

-- TODO: this should probably have a better name.
-- Alternatively what we save in the
-- model state should just be a property.
asserts :: ModelState state -> Property
asserts finalState = foldr (.&&.) (property True) [ counterexample ("assertSpec failed: " ++ s) b
                                                  | (s, b) <- finalState ^. assertions ]
