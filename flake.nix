{
  description = "Nix support for developing share";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    let
      ghc-version = "928";
      systemAttrs = flake-utils.lib.eachDefaultSystem (system:
        let
          pkgs = nixpkgs.legacyPackages."${system}".extend self.overlay;
          ghc = pkgs.haskell.packages."ghc${ghc-version}";
          nativePackages = pkgs.lib.optionals pkgs.stdenv.isDarwin
            (with pkgs.darwin.apple_sdk.frameworks; [ Cocoa ]);


          share-env = pkgs.mkShell {
            packages = let exports = self.packages."${system}";
            in with pkgs;
            [
              exports.stack
              exports.hls
              exports.ghc
              exports.ormolu
              docker
              docker-compose
              glibcLocales
              gmp
              pkg-config
              postgresql
              redis
              sqlite
              zlib
            ] ++ nativePackages;
            # workaround for https://gitlab.haskell.org/ghc/ghc/-/issues/11042
            shellHook = ''
              export LD_LIBRARY_PATH=${pkgs.zlib}/lib:$LD_LIBRARY_PATH
            '';
          };
        in {
          apps.repl = flake-utils.lib.mkApp {
            drv =
              nixpkgs.legacyPackages."${system}".writeShellScriptBin "repl" ''
                confnix=$(mktemp)
                echo "builtins.getFlake (toString $(git rev-parse --show-toplevel))" >$confnix
                trap "rm $confnix" EXIT
                nix repl $confnix
              '';
          };

          pkgs = pkgs;

          devShells.default = share-env;

          packages = {
            ormolu = pkgs.ormolu;
            ghc = pkgs.haskell.compiler."ghc${ghc-version}";
            hls = ghc.haskell-language-server;
            hls-call-hierarchy-plugin = ghc.hls-call-hierarchy-plugin;
            stack = pkgs.unison-stack;
            devShell = self.devShells."${system}".default;
          };

          defaultPackage = self.packages."${system}".devShell;
        });
      topLevelAttrs = {
        overlay = final: prev: {
          unison-stack = prev.symlinkJoin {
            name = "stack";
            paths = [ final.stack ];
            buildInputs = [ final.makeWrapper ];
            postBuild = let
              flags = [ "--no-nix" "--system-ghc" "--no-install-ghc" ];
              add-flags =
                "--add-flags '${prev.lib.concatStringsSep " " flags}'";
            in ''
              wrapProgram "$out/bin/stack" ${add-flags}
            '';
          };
        };
      };
      in systemAttrs // topLevelAttrs;

}
