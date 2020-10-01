let

  nixpkgsRev = "502845c3e31ef3de0e424f3fcb09217df2ce6df6";
  compilerVersion = "ghc884";
  compilerSet = pkgs.haskell.packages."${compilerVersion}";

  githubTarball = owner: repo: rev:
    builtins.fetchTarball { url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz"; };

  pkgs = import (githubTarball "NixOS" "nixpkgs" nixpkgsRev) { inherit config; };
  gitIgnore = pkgs.nix-gitignore.gitignoreSourcePure;

  config = {
    packageOverrides = super: let self = super.pkgs; in rec {
      haskell = super.haskell // {
        packageOverrides = self: super: {
          realworld-api = super.callCabal2nix "realworld-api" (gitIgnore [./.gitignore] ./api) {}; #
          realworld-backend = super.callCabal2nix "realworld-backend" (gitIgnore [./.gitignore] ./backend/common) {}; #
          realworld-backend-fused-effects = super.callCabal2nix "realworld-backend-fused-effects" (gitIgnore [./.gitignore] ./backend/fused-effects) {}; #
        };
      };
    };
  };

in {
  inherit pkgs compilerSet;
}
