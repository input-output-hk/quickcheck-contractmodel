module Test.QuickCheck.ContractModel.Internal.Utils where

import Test.QuickCheck.ContractModel.Internal.Common
import Data.Map (Map)
import Data.Map qualified as Map

import Cardano.Api

getTxOuts :: Tx Era -> [TxOut CtxTx Era]
getTxOuts (getTxBody -> TxBody content) = txOuts content

getTxInputs :: Tx Era -> UTxO Era -> [TxOut CtxUTxO Era]
getTxInputs (getTxBody -> TxBody content) (UTxO utxo) =
  [ txOut
  | (txIn, _) <- txIns content
  , Just txOut <- [Map.lookup txIn utxo]
  ]

bucket :: (Num a, Ord a, Show a, Integral a) => a -> a -> [String]
bucket _ 0 = ["0"]
bucket size n | n < size = [ "<" ++ show size ]
              | size <= n, n < size*10 = [bucketIn size n]
              | otherwise = bucket (size*10) n
  where bucketIn size n = let b = n `div` size in show (b*size) ++ "-" ++ show (b*size+(size - 1))
