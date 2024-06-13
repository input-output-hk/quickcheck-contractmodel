module Test.QuickCheck.ContractModel.Internal.Common (Era, LedgerEra, era) where

import Cardano.Api
import Cardano.Api.Shelley

type Era = ConwayEra
type LedgerEra = ShelleyLedgerEra Era

era :: ShelleyBasedEra Era
era = shelleyBasedEra @Era
