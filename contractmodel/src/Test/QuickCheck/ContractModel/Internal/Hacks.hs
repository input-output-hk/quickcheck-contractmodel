module Test.QuickCheck.ContractModel.Internal.Hacks where

import Cardano.Api

import Data.Data

-- TODO: this *needs* to be upstreamed - these implementations don't work and are
-- only here to make the type checker happy!
-- TODO: if we run into issues because of this we might have to resort to
-- using the `Value` type from `plutus-core` instead - which would be yuck and
-- would start mixing different APIs.
instance Data Quantity
instance Data Value

instance Ord (AddressInEra BabbageEra)
