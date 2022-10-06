module Test.QuickCheck.ContractModel.Internal.Common (Era, era) where

import Cardano.Api

import Test.QuickCheck.ContractModel.Internal.Hacks()

type Era = BabbageEra

era :: ShelleyBasedEra Era
era = shelleyBasedEra @Era
