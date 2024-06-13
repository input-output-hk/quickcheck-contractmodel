{ repoRoot, inputs, pkgs, lib, system }:

_cabalProject:

{
  name = "qc-cm";
  packages = [
    pkgs.ghcid
    pkgs.haskellPackages.hoogle
  ];

  preCommit = {
    nixpkgs-fmt.enable = true;
    cabal-fmt.enable = false;
    fourmolu.enable = false;
    fourmolu.extraOptions = "-o -XImportQualifiedPost -o -XTypeApplications -o -XPatternSynonyms";
  };
}
