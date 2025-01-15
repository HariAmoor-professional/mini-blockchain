{
  description = "Hari Amoor's implementation of the Ouroboros BFT protocol (technical challenge)";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = inputs.nixpkgs.lib.systems.flakeExposed;
      imports = with inputs; [
        haskell-flake.flakeModule
        treefmt-nix.flakeModule
      ];
      perSystem = { self', system, lib, config, pkgs, ... }: {
        haskellProjects.default = {
          devShell.hlsCheck.enable = false;
          autoWire = [ "packages" "apps" "checks" ]; # Wire all but the devShell
        };

        treefmt = {
          projectRootFile = "flake.nix";

          programs = {
            fourmolu.enable = true;
            nixpkgs-fmt.enable = true;
            cabal-fmt.enable = true;
            just.enable = true;
            yamlfmt.enable = true;
            mdformat.enable = true;
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
