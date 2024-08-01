{ repoRoot, inputs, pkgs, lib, system }:

let

  project = lib.iogx.mkHaskellProject {

    cabalProject = pkgs.haskell-nix.cabalProject' {
      name = "quickcheck-contract-model";
      src = ../.;
      compiler-nix-name = lib.mkDefault "ghc962";
      shell.withHoogle = false;
      inputMap = {
        "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.iogx.inputs.CHaP;
      };
    };

    shellArgs = _cabalProject: {
      name = "qc-cm";
      preCommit = {
        nixpkgs-fmt.enable = true;
        cabal-fmt.enable = false;
        fourmolu.enable = false;
        fourmolu.extraOptions = "-o -XImportQualifiedPost -o -XTypeApplications -o -XPatternSynonyms";
      };
    };
  };

in

[
  (
    project.flake
  )
]
