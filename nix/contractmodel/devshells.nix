{ inputs, cell }: 
{
  default = inputs.devx.devShells.ghc8107; 

  # TODO uncomment once devx has been standardized 
  # extended = inputs.std.lib.dev.mkShell {
  #   name = "qc-c";
  #   imports = [ inputs.devx.devshellProfiles.ghc8107 ];
  #   packages = [
  #     # Custom stuff 
  #   ];
  # };
}