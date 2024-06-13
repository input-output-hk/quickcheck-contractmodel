{ repoRoot, inputs, pkgs, lib, system }:

let

  sha256map = { };

  modules = [
    ({ config, ... }: {
      packages = { assignment.ghcOptions = [ "-Werror" ]; };
    })
  ];

  cabalProject = pkgs.haskell-nix.cabalProject' {
    inherit modules sha256map;
    src = ../.;
    name = "qc-cm";
    compiler-nix-name = "ghc96";
    inputMap = {
      "https://input-output-hk.github.io/cardano-haskell-packages" =
        inputs.CHaP;
    };
    shell.withHoogle = false;
  };

  project = lib.iogx.mkHaskellProject {
    inherit cabalProject;
    shellArgs = repoRoot.nix.shell;
  };

in
project
