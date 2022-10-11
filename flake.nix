{
  description = "quickcheck-contractmodel";

  inputs = {
    devx.url = "github:input-output-hk/devx"; 
    nixpkgs.follows = "devx/nixpkgs";
    std = {
      url = "github:divnix/std";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs:
    inputs.std.growOn {
      inherit inputs;
      cellsFrom = ./nix;
      cellBlocks = [
        (inputs.std.devshells "devshells")
      ];
    }
    {
      devShells = inputs.std.harvest inputs.self [ "contractmodel" "devshells" ];
    };
 
  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" "https://hydra.iohk.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = true;
  };
}
