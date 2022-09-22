{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Test.QuickCheck.ContractModel.Symbolics where

import Cardano.Api

import Test.QuickCheck.StateModel
import Test.QuickCheck.ContractModel.Internal.Common ()

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Data
import Data.Maybe

-- | A symbolic token is a token that exists only during ContractModel generation time
data SymToken = SymToken { symVar    :: Var (Map String AssetId)
                         , symVarIdx :: String
                         } deriving (Ord, Eq, Data)

-- | A symbolic value is a combination of a real value and a value associating symbolic
-- tokens with an amount
data SymValue = SymValue { symValMap     :: Map SymToken Quantity
                         , actualValPart :: Value
                         } deriving (Show, Data)

instance Show SymToken where
  show (SymToken (Var i) n) = "tok" ++ show i ++ "." ++ n

instance Semigroup SymValue where
  (SymValue m v) <> (SymValue m' v') = SymValue (Map.unionWith (+) m m') (v <> v')

instance Monoid SymValue where
  mempty = SymValue mempty mempty

instance Eq SymValue where
  (SymValue m v) == (SymValue m' v') = Map.filter (/= 0) m == Map.filter (/= 0) m'
                                     && v == v'

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
