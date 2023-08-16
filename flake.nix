{
  description = "Hari Amoor's implementation of the Ouroboros BFT protocol (technical challenge)";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-nix.flakeModule
      ];
      perSystem = { self', system, lib, config, pkgs, ... }: {
        haskellProjects.default = {
          devShell.hlsCheck.enable = false;
          autoWire = [ "packages" "apps" "checks" ]; # Wire all but the devShell
        };

        treefmt.config = {
          projectRootFile = "flake.nix";

          programs = {
            /*
              We prefer fourmolu b/c we don't want to be tied
              down to Tweag's formatting preferences
            */
            ormolu.enable = true;
            ormolu.package = pkgs.haskellPackages.fourmolu;

            nixpkgs-fmt.enable = true;
            cabal-fmt.enable = true;
            hlint.enable = true;
          };

          settings.formatter.ormolu = {
            options = [
              "--ghc-opt"
              "-XImportQualifiedPost"
            ];
          };
        };

        packages.default = self'.packages.ping-pong;
        apps.default = self'.apps.ping-pong;

        devShells.default = pkgs.mkShell {
          name = "haskell-template";
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
            config.treefmt.build.devShell
          ];
          nativeBuildInputs = with pkgs; [
            just
          ];
        };
      };
    };
}
