# Emulator API

QuickCheck ContractModel is a layer on top of quckcheck-dynamic to model Plutus contracts.
This results in QuickCheck generating test cases for those contracts according to the specified model.
In order to run those test caes, one needs to emulate the Cardano blockhain.

There is a generator in plutus-apps, but if you have your own requirements on the emulator, then you can use your own emulator.
It requires that you have implemented a Cardano.API compatible interface. The contract model needs the following

## API

You need to implement your emulator as a monad with an instance for
`HasChainIndex` which requires a function `getChainIndex` to get a chain index in your emulator:

```
data ChainIndex = ChainIndex
  { before       :: ChainState
  , after        :: ChainState
  , transactions :: [TxInState]
  , networkId    :: NetworkId
  }

data ChainState = ChainState
  { slot :: SlotNo
  , utxo :: UTxO Era
  }

data TxInState = TxInState
  { tx         :: Tx Era
  , chainState :: ChainState
  , accepted   :: Bool
  }

```


You also need to implement an instance for `IsRunnable` which requires a function `awaitSlot` to reach a certain slot in your emulated blockchain.

See:
- [ChainIndex.hs](contractmodel/src/Test/QuickCheck/ContractModel/Internal/ChainIndex.hs)
- [Inner.hs](contractmodel/src/Test/QuickCheck/ContractModel/Internal.hs#L74)
