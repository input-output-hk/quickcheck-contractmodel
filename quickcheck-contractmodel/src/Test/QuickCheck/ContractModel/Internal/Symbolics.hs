{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Test.QuickCheck.ContractModel.Internal.Symbolics where

import Cardano.Api
import Control.Lens

import Test.QuickCheck.StateModel
import Test.QuickCheck.ContractModel.Internal.Common (Era)

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Maybe
import Data.Foldable
import Data.Function
import Barbies
import Barbies.Constraints
import Text.PrettyPrint.HughesPJClass hiding ((<>))

------------------------------------------------------------------------
-- The Barbie
------------------------------------------------------------------------

data SymIndexF f = SymIndex { _tokens :: f AssetId
                            , _utxos  :: f (TxOut CtxUTxO Era)
                            } deriving stock Generic
                              deriving anyclass (ConstraintsB, FunctorB, ApplicativeB, TraversableB)
makeLenses ''SymIndexF

deriving instance AllBF Show f SymIndexF => Show (SymIndexF f)
deriving instance AllBF Eq f SymIndexF => Eq (SymIndexF f)

class HasSymbolicRep t where
  symIndexL :: Lens' (SymIndexF f) (f t)
  symPrefix :: String

instance HasSymbolicRep (TxOut CtxUTxO Era) where
  symIndexL = utxos
  symPrefix = "txOut"

instance HasSymbolicRep AssetId where
  symIndexL = tokens
  symPrefix = "tok"

-- Semigroup and Monoids --------------------------------------------------

bmapConst :: FunctorB b => (forall a. f a -> c) -> b f -> Container b c
bmapConst f b = Container $ bmap (Const . f) b

mappendSymIndexF :: forall f. (AllBF Semigroup f SymIndexF, Show (SymIndexF f))
                 => (forall a. f a -> Set String)
                 -> SymIndexF f
                 -> SymIndexF f
                 -> SymIndexF f
mappendSymIndexF toSet s s'
  | and (Set.disjoint <$> bmapConst toSet s
                      <*> bmapConst toSet s') = bzipWithC @(ClassF Semigroup f) (<>) s s'
  | otherwise = error $ unlines [ "Non-unique SymIndexF", show s, show s' ]

instance Semigroup SymIndex where
  (<>) = mappendSymIndexF Map.keysSet

instance Semigroup SymCreationIndex where
  (<>) = mappendSymIndexF getConst

instance Semigroup SymCollectionIndex where
  (<>) = bzipWith (<>)

instance (AllBF Monoid f SymIndexF, Semigroup (SymIndexF f)) => Monoid (SymIndexF f) where
  mempty = bmempty

------------------------------------------------------------------------
-- Applications
------------------------------------------------------------------------

-- | For an assumed variable, what's the mapping of String indices to
-- underlying actual values. This is what is returned by a contract model
-- action when it runs.
type SymIndex = SymIndexF (Map String)

symIndex :: HasSymbolicRep t => String -> t -> SymIndex
symIndex s t = mempty & symIndexL . at s .~ Just t

-- | For a given action, what are the String indices used to construct
-- symbolic variables when this action ran. NOTE: this purposefully does
-- not include the variable because we might want to fake the variable
-- with `Var 0` in some cases. See comment somewhere for why this is
-- safe...
type SymCreationIndex = SymIndexF (Const (Set String))

toCreationIndex :: SymIndex -> SymCreationIndex
toCreationIndex = bmap (Const . Map.keysSet)

createIndex :: forall t. HasSymbolicRep t
            => String -> SymCreationIndex
createIndex s = mempty & symIndexL @t .~ Const (Set.singleton s)

showCreateIndex :: SymCreationIndex -> String
showCreateIndex = show . Set.toList . fold . Container . bmapC @HasSymbolicRep addPrefix
  where
    addPrefix :: forall t. HasSymbolicRep t => Const (Set String) t -> Const (Set String) t
    addPrefix (Const set) = Const $ Set.mapMonotonic ((symPrefix @t ++ ".") ++) set

-- | What symbolic variables have been created in a given run of the
-- `Spec` monad?
type SymCollectionIndex = SymIndexF SymSet

newtype SymSet t = SymSet { unSymSet :: Set (Symbolic t) }
  deriving stock Generic
  deriving newtype (Semigroup, Monoid)

deriving instance Show (Symbolic t) => Show (SymSet t)

symCollect :: HasSymbolicRep t
           => Symbolic t -> SymCollectionIndex
symCollect s = mempty & symIndexL .~ (SymSet $ Set.singleton s)

makeSymCollection :: SymCreationIndex -> Var SymIndex -> SymCollectionIndex
makeSymCollection ci v = bmap (SymSet . Set.mapMonotonic (Symbolic v) . getConst) ci

symCollectionSubset :: SymCollectionIndex -> SymCollectionIndex -> Bool
symCollectionSubset s0 s1 = and . Container $ bzipWith ((Const .) . Set.isSubsetOf `on` unSymSet) s0 s1

------------------------------------------------------------------------
-- Symbolic representations
------------------------------------------------------------------------

data Symbolic t = Symbolic { symVar :: Var SymIndex
                           , symVarIdx :: String
                           } deriving stock (Eq, Ord)

type SymbolicSemantics = forall t. HasSymbolicRep t => Symbolic t -> t

instance HasSymbolicRep t => Show (Symbolic t) where
  show (Symbolic v n) = symPrefix @t ++ "." ++ show v ++ "." ++ n

getSymbolics :: forall t. HasSymbolicRep t
             => SymCreationIndex -> Var SymIndex -> Set (Symbolic t)
getSymbolics idx v = makeSymCollection idx v ^. symIndexL @t . to unSymSet

instance HasVariables (Symbolic t) where
  getAllVariables = getAllVariables . symVar

lookupSymbolic :: HasSymbolicRep t => SymIndex -> Symbolic t -> Maybe t
lookupSymbolic idx s = idx ^. symIndexL . at (symVarIdx s)

-- | A SymTxOut is a `TxOut CtxUTxO Era` that is only available at runtime
type SymTxOut = Symbolic (TxOut CtxUTxO Era)

-- | A symbolic token is a token that is only available at runtime
type SymToken = Symbolic AssetId

-- Symbolic values --------------------------------------------------------

-- | A symbolic value is a combination of a real value and a value associating symbolic
-- tokens with an amount
data SymValue = SymValue { symValMap     :: Map SymToken Quantity
                         , actualValPart :: Value
                         }
  deriving stock (Show, Generic)


instance Semigroup SymValue where
  (SymValue m v) <> (SymValue m' v') = SymValue (Map.unionWith (+) m m') (v <> v')

instance Monoid SymValue where
  mempty = SymValue mempty mempty

instance Eq SymValue where
  (SymValue m v) == (SymValue m' v') = Map.filter (/= 0) m == Map.filter (/= 0) m'
                                     && v == v'

pPrintValue :: Value -> Doc
pPrintValue val = braces $ sep $ punctuate comma
  [ text (show v) <+> pPrintAssetId asset | (asset, v) <- valueToList val ]

pPrintAssetId :: AssetId -> Doc
pPrintAssetId AdaAssetId            = text "Lovelace"
pPrintAssetId (AssetId policy name) = text $ take 8 (tail $ show policy) ++ ":" ++ filter (/= '"') (show name)

instance Pretty SymValue where
  pPrint (SymValue sym val) = braces $ sep $ punctuate comma $
    [ text (show v) <+> text (show tok)     | (tok, v)   <- Map.toList sym  ] ++
    [ text (show v) <+> pPrintAssetId asset | (asset, v) <- valueToList val ]

-- | Check if a symbolic value is zero
symIsZero :: SymValue -> Bool
symIsZero (SymValue m v) =
  and [ all (==0) m
      , all ((==0) . snd) (valueToList v)
      ]

-- | Check if one symbolic value is less than or equal to another
symLeq :: SymValue -> SymValue -> Bool
symLeq (SymValue m v) (SymValue m' v') =
  and [ all ((<= 0) . snd) (valueToList $ v <> negateValue v')
      , all (<=0) (Map.unionWith (+) m (negate <$> m'))
      ]

-- | Using a semantics function for symbolic tokens, convert a SymValue to a Value
toValue :: (SymToken -> AssetId) -> SymValue -> Value
toValue symTokenMap (SymValue m v) = v <> valueFromList [ (symTokenMap t, v) | (t, v) <- Map.toList m ]

-- | Invert a sym token mapping to turn a Value into a SymValue,
-- useful for error reporting
toSymVal :: (AssetId -> Maybe SymToken) -> Value -> SymValue
toSymVal invSymTokenMap v =
  let aidMap = [ (ai, i) | (ai, i) <- valueToList v ]
  -- TODO: prettier to do with a fold
  in SymValue (Map.fromList [ (tn, i) | (ai, i) <- aidMap, tn <- maybeToList $ invSymTokenMap ai ])
              (valueFromList [ (ai, i) | (ai, i) <- aidMap, invSymTokenMap ai == Nothing ])

-- Negate a symbolic value
inv :: SymValue -> SymValue
inv (SymValue m v) = SymValue (negate <$> m) (negateValue v)

class SymValueLike v where
  toSymValue :: v -> SymValue

class TokenLike t where
  -- | Get the value of a specific token in a `SymValue`
  symAssetIdValueOf :: SymValue -> t -> Quantity
  -- | Convert a token and an amount to a `SymValue`
  symAssetIdValue :: t -> Quantity -> SymValue

instance SymValueLike Value where
  toSymValue = SymValue mempty

instance SymValueLike SymValue where
  toSymValue = id

instance TokenLike SymToken where
  symAssetIdValueOf (SymValue svm _) t = sum $ Map.lookup t svm

  symAssetIdValue _ 0 = SymValue mempty mempty
  symAssetIdValue t i = SymValue (Map.singleton t i) mempty

instance TokenLike AssetId where
  symAssetIdValueOf = selectAsset . actualValPart
  symAssetIdValue t i = toSymValue $ valueFromList [(t, i)]
