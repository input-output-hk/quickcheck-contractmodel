module Test.QuickCheck.ContractModel.Internal.Hacks where

import Cardano.Api

-- TODO: this *needs* to be upstreamed - these implementations don't work and are
-- only here to make the type checker happy!
instance Ord (AddressInEra BabbageEra)
