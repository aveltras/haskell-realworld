let

  nixpkgsRev = "502845c3e31ef3de0e424f3fcb09217df2ce6df6";
  compilerVersion = "ghc884";
  compilerSet = pkgs.haskell.packages."${compilerVersion}";

  githubTarball = owner: repo: rev:
    builtins.fetchTarball { url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz"; };

  pkgs = import (githubTarball "NixOS" "nixpkgs" nixpkgsRev) { inherit config; };
  gitIgnore = pkgs.nix-gitignore.gitignoreSourcePure;
  haskellLib = pkgs.haskell.lib;

  beamSrc = githubTarball "haskell-beam" "beam" "efd464b079755a781c2bb7a2fc030d6c141bbb8a";

  config = {
    packageOverrides = super: let self = super.pkgs; in rec {
      haskell = super.haskell // {
        packageOverrides = self: super: with haskellLib; {
          realworld-api = super.callCabal2nix "realworld-api" (gitIgnore [./.gitignore] ./api) {}; #
          realworld-backend = super.callCabal2nix "realworld-backend" (gitIgnore [./.gitignore] ./backend) {}; #

          beam-core = dontCheck (super.callCabal2nix "beam-core" "${beamSrc}/beam-core" {});
          beam-postgres = dontCheck (super.callCabal2nix "beam-core" "${beamSrc}/beam-postgres" {});
          beam-migrate = dontCheck (super.callCabal2nix "beam-core" "${beamSrc}/beam-migrate" {});

          hasql-th = unmarkBroken (dontCheck super.hasql-th);
          postgresql-syntax = unmarkBroken (dontCheck super.postgresql-syntax);
          megaparsec = self.callHackageDirect 
              { pkg = "megaparsec";
                ver = "9.0.0";
                sha256 = "VHTZlu8Xc8pmrvUk75PLN90q9Ly0ampyJbTEqq9jeA4=";
              } {};

          squeal-postgresql = dontCheck (self.callHackageDirect 
              { pkg = "squeal-postgresql";
                ver = "0.7.0.1";
                sha256 = "HAGueVnFBJZNuAfZBccCWXWjoqWPMqoP5K0MgmgvFnI=";
              } {});
        };
      };
    };
  };

in {
  inherit pkgs compilerSet;
}
