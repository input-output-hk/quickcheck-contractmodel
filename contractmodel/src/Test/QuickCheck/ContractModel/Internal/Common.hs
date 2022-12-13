module Test.QuickCheck.ContractModel.Internal.Common (Era, LedgerEra, era) where

import Cardano.Api
import Cardano.Api.Shelley

type Era = BabbageEra
type LedgerEra = ShelleyLedgerEra Era

era :: ShelleyBasedEra Era
era = shelleyBasedEra @Era

-- TODO: remove once https://github.com/input-output-hk/cardano-node/pull/4528 has made it into all
--       dependencies
instance Ord (AddressInEra era) where
  compare (AddressInEra ByronAddressInAnyEra addr1)
          (AddressInEra ByronAddressInAnyEra addr2) = compare addr1 addr2

  compare (AddressInEra ShelleyAddressInEra{} addr1)
          (AddressInEra ShelleyAddressInEra{} addr2) = compare addr1 addr2

  compare (AddressInEra ByronAddressInAnyEra _)
          (AddressInEra ShelleyAddressInEra{} _) = LT

  compare (AddressInEra ShelleyAddressInEra{} _)
          (AddressInEra ByronAddressInAnyEra _) = GT

