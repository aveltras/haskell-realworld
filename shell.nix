
with (import ./default.nix);

compilerSet.shellFor {
  packages = p: with p; [
    realworld-api
    realworld-backend
    realworld-backend-fused-effects
  ];
  buildInputs = with compilerSet; [
    cabal-install
    ghc
    haskell-language-server
    hlint
  ];
}
