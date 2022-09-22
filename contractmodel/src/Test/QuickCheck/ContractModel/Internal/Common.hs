module Test.QuickCheck.ContractModel.Internal.Common (Era, bucket) where

import Cardano.Api

import Test.QuickCheck.ContractModel.Internal.Hacks()

type Era = BabbageEra

bucket :: (Num a, Ord a, Show a, Integral a) => a -> a -> [String]
bucket _ 0 = ["0"]
bucket size n | n < size = [ "<" ++ show size ]
              | size <= n, n < size*10 = [bucketIn size n]
              | otherwise = bucket (size*10) n
  where bucketIn size n = let b = n `div` size in show (b*size) ++ "-" ++ show (b*size+(size - 1))
