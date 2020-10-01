
with (import ./default.nix);

compilerSet.shellFor {
  packages = p: with p; [
    realworld-api
    realworld-backend
  ];
  buildInputs = with compilerSet; [
    cabal-install
    ghc
    haskell-language-server
    hlint
    (pkgs.writeShellScriptBin "dev" ''
      ${pkgs.ghcid}/bin/ghcid -c "cabal repl realworld-backend" -W -T :main ''${@:2}
    '')
  ];
}
