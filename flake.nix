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
    extra-substituters = [ 
      "https://cache.iog.io" 
      "https://hydra.iohk.io" 
      "https://cache.zw3rk.com"
    ];
    extra-trusted-public-keys = [ 
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" 
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    ];
    allow-import-from-derivation = true;
  };
}
