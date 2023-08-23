# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#38-nixformattersnix

{
  nixpkgs-fmt.enable = true;
  cabal-fmt.enable = false;
  fourmolu.enable = false;
  fourmolu.extraOptions = "-o -XImportQualifiedPost -o -XTypeApplications -o -XPatternSynonyms";
}
