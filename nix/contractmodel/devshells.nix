{ inputs, cell }: 
{
  default = inputs.devx.devShells.ghc8107.overrideAttrs (attrs: {
    buildInputs = attrs.buildInputs ++ [
      inputs.nixpkgs.lzma.dev
    ];
  });
}