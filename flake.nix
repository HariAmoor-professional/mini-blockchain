{
  description = "srid/haskell-template: Nix template for Haskell projects";
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
        # Our only Haskell project. You can have multiple projects, but this template
        # has only one.
        # See https://github.com/srid/haskell-flake/blob/master/example/flake.nix
        haskellProjects.default =
          let
            compiler = "ghc96";
          in
          {
            # The base package set (this value is the default)
            # basePackages = pkgs.haskell.packages.${compiler};

            # Packages to add on top of `basePackages`
            packages = {
              # Add source or Hackage overrides here
              # (Local packages are added automatically)
              /*
            aeson.source = "1.5.0.0" # Hackage version
            shower.source = inputs.shower; # Flake input
              */
              # unix.source = "2.7.3";
              # unix-compat.source = "0.6";
            };

            # Add your package overrides here
            settings = {
              /*
            haskell-template = {
              haddock = false;
            };
            aeson = {
              check = false;
            };
              */
            };

            # Development shell configuration
            devShell = {
              hlsCheck.enable = false;
            };

            # What should haskell-flake add to flake outputs?
            autoWire = [ "packages" "apps" "checks" ]; # Wire all but the devShell
          };

        # Auto formatters. This also adds a flake check to ensure that the
        # source tree was auto formatted.
        treefmt.config = {
          projectRootFile = "flake.nix";

          programs.ormolu.enable = true;
          programs.nixpkgs-fmt.enable = true;
          programs.cabal-fmt.enable = true;
          programs.hlint.enable = true;

          # We use fourmolu
          programs.ormolu.package = pkgs.haskellPackages.fourmolu;
          settings.formatter.ormolu = {
            options = [
              "--ghc-opt"
              "-XImportQualifiedPost"
            ];
          };
        };

        # Default package & app.
        packages.default = self'.packages.challenge;
        apps.default = self'.apps.challenge;

        # Default shell.
        devShells.default = pkgs.mkShell {
          name = "haskell-template";
          # See https://zero-to-flakes.com/haskell-flake/devshell#composing-devshells
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
