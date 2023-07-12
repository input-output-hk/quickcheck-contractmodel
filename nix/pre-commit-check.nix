# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#38-nixpre-commit-checknix

{
  cabal-fmt.enable = false;
  nixpkgs-fmt.enable = true;
  fourmolu.enable = false;
  fourmolu.extraOptions = "-o -XImportQualifiedPost -o -XTypeApplications -o -XPatternSynonyms";
}
