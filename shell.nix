
with (import ./default.nix);

compilerSet.shellFor {
  packages = p: [p.realworld-api];
  buildInputs = with compilerSet; [
    cabal-install
    ghc
    haskell-language-server
    hlint
  ];
}
