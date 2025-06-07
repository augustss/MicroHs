{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    haskell-flake.url = "github:srid/haskell-flake";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs@{ flake-parts, ... }:
    # https://flake.parts/module-arguments.html
    flake-parts.lib.mkFlake { inherit inputs; }
    (top@{ config, withSystem, moduleWithSystem, ... }: {
      imports = [
        # Optional: use external flake logic, e.g.
        # inputs.foo.flakeModules.default
        inputs.haskell-flake.flakeModule
      ];
      flake = { };
      systems = [
        # systems for which you want to build the `perSystem` attributes
        "x86_64-linux"
        # ...
      ];
      perSystem = { config, pkgs, ... }: {
        devShells.default = pkgs.mkShell {
          packages = [
            pkgs.gnumake
            pkgs.gcc
            pkgs.tinycc
            (pkgs.haskell.packages.ghc910.ghcWithPackages
              (p: with p; [ cabal-install haskell-language-server ]))
          ];
        };
        # Recommended: move all package definitions here.
        # e.g. (assuming you have a nixpkgs input)
        # packages.foo = pkgs.callPackage ./foo/package.nix { };
        # packages.bar = pkgs.callPackage ./bar/package.nix {
        #   foo = config.packages.foo;
        # };
      };
    });

}
