module Test.QuickCheck.ContractModel.Internal.Common (Era, era) where

import Cardano.Api

type Era = BabbageEra

era :: ShelleyBasedEra Era
era = shelleyBasedEra @Era
