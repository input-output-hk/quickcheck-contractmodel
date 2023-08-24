# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#31-flakenix

{
  description = "Interface to quickcheck-dynamic for Developers of Plutus Scripts";


  inputs = {
    iogx.url = "github:input-output-hk/iogx";
    iogx.inputs.hackage.follows = "hackage";

    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };
  };


  outputs = inputs: inputs.iogx.lib.mkFlake {
    inherit inputs;
    repoRoot = ./.;
    systems = [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ];
  };


  nixConfig = {

    extra-substituters = [
      "https://cache.iog.io"
    ];

    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];

    allow-import-from-derivation = true;
  };
}
