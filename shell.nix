
with (import ./default.nix);

compilerSet.shellFor {
  packages = p: [];
  buildInputs = with compilerSet; [
    cabal-install
    ghc
    haskell-language-server
    hlint
  ];
}
