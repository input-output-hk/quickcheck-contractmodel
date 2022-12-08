# Contract Model #

This repository provides an interface to
[quickcheck-dynamic](https://github.com/input-output-hk/quickcheck-dynamic/)
for developers of Plutus scripts. The main purpose of this repository is to offer
an alternative to testing Plutus scripts with quickcheck via the
[plutus-apps](https://github.com/input-output-hk/plutus-apps/) repository.
If you use the plutus-apps repository for development of your scripts, we refer to the
[tutorials](https://plutus-apps.readthedocs.io/en/latest/plutus/tutorials/contract-models.html).
You do not need this repository, it is a dependency of plutus-apps.

*TODO: we need some form of "how to get started" thing here*

## Automatic test case generation ##

This repository provides you with the means to automatically generate test cases for your Cardano smart
contracts. To do this it uses a model of the contract that you provide as an instance of the
`ContactModel` class. This model describes for each action of your contract (e.g. contract endpoint
or functionality) what the behaviour of that action is on an abstracted contract and blockchain state.
It is very similar to the models provided in this
[tutorial](https://plutus-apps.readthedocs.io/en/latest/plutus/tutorials/contract-models.html).

If you want to be independent of plutus-apps, use your own way and languages designing Plutus scripts,
then this repository offers an emulator-generic interface for testing Cardano contracts on top of
quickcheck-dynamic. There are standard properties that quickcheck tests for. The models provide
automatic shrinking of failing test cases, such that your debug effort is minimized.

## Actions and Transactions ##

The contract model describes the actions users can perform with your contract. Many of these actions
will result in the generation of a transaction that is then posted to the blockchain.
However, you can also add actions to the model to wait a certain number of blocks or to put funds or
tokens into a wallet not involving the plutus script under test. The actions described in the model
have to be connected to the world in which you write your contract via the interface of a
`perform` function. Think of it like you write your actions abstract in the model world,
like `Pay` and `Redeem`, whereas you build your transactions via some code that comes up with the
actual transaction that you have written as part of your contract API. If that API is Haskell, then
the effort to write the `perform` function is rather limited. However, if you have written it in a
different language, you need to bridge between Haskell and that language.

You have to provide an interface between the abstract operations in the contract model and the
off-chain code in your project that creates the actual transactions.

## Blockchain Emulation ##

In order to test your contract, the transactions you create are posted to a blockchain emulator.
You can use any emulator you fancy as long as it is Cardano.API compatible. You could even use a
testnet, but be aware that this can be very slow if you run hundreds of transactions in your tests.
The API you need to implement talking to your emulator of choice is documented in
[EmulatorAPI.md](EmulatorAPI.md).

## Coverage data ##

When quickcheck runs your property, it collect on-chain coverage data
(see [this](https://plutus-apps.readthedocs.io/en/latest/plutus/tutorials/contract-models.html#measuring-coverage-of-on-chain-code)).
That is, it collects statistics which part of the validators are used during testing, but most
importantly, which validator parts are never used during testing. If certain parts of the validator
are never triggered during testing, then testing may not be through enough.

We use the standard plutus compiler to insert coverage tags in the on-chain code. If you do not
use the plutus compiler and still want to collect on-chain validator coverage information, you’ll
need to add those in a different way.

## Standard properties ##

There are standard properties for checking that your contract cannot get in a state in which funds are
locked and nobody can get those funds out. You can re-use those properties. You can add your own
additional unit tests, if so required.

*TODO: Something about double satisfaction and threat models later*
