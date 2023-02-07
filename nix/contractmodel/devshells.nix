{ inputs, cell }: 
let 
  pkgs = import inputs.nixpkgs {};
in 
{
  default = inputs.devx.devShells.ghc8107.overrideAttrs (attrs: {
    buildInputs = attrs.buildInputs ++ [
      inputs.nixpkgs.lzma.dev
    ];
  });

  # TODO uncomment once devx has been standardized 
  # extended = inputs.std.lib.dev.mkShell {
  #   name = "qc-c";
  #   imports = [ inputs.devx.devshellProfiles.ghc8107 ];
  #   packages = [
  #     # Custom stuff 
  #   ];
  # };
}