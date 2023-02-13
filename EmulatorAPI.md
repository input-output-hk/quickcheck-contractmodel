# Emulator API

QuickCheck ContractModel is a layer on top of quckcheck-dynamic to model Plutus contracts.
This results in QuickCheck generating test cases for those contracts according to the specified model.
In order to run those test caes, one needs to emulate the Cardano blockhain.

There is an emulator in plutus-apps, but if you have your own requirements on the emulator,
you can use your own emulator. However, to use `quickcheck-contractmodel` your emulator needs a
`Cardano.API` compatible interface.

Specifically, you need to implement your emulator as a monad with an instance for `HasChainIndex` which
requires a function `getChainIndex` to get a chain index in your emulator:

```
data ChainIndex = ChainIndex
  { transactions :: [TxInState]
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

You also need to implement an instance for `IsRunnable` which requires a function `awaitSlot` to reach
a given slot on your emulated blockchain.

See:
- [ChainIndex.hs](contractmodel/src/Test/QuickCheck/ContractModel/Internal/ChainIndex.hs)
- [Internal.hs](contractmodel/src/Test/QuickCheck/ContractModel/Internal.hs#L79)
