{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts = { url = "github:hercules-ci/flake-parts"; inputs.nixpkgs.follows = "nixpkgs"; };
    haskell-flake.url = "github:srid/haskell-flake";
    lint-utils = { url = "git+https://gitlab.homotopic.tech/nix/lint-utils"; inputs.nixpkgs.follows = "nixpkgs"; };
  };

  outputs = { self, nixpkgs, flake-parts, haskell-flake, lint-utils }:
    flake-parts.lib.mkFlake { inherit self; } {
      systems = [ "x86_64-linux" ];
      imports = [ haskell-flake.flakeModule ];
      perSystem = { self', system, pkgs, lib, ... }: {
        haskellProjects.default = {
          haskellPackages = pkgs.haskell.packages.ghc924;
          root = self;
          overrides = self: super: with pkgs.haskell.lib; {
            shh = doJailbreak super.shh;
            lens-aeson = dontCheck (self.callHackage "lens-aeson" "1.2.1" { });
          };
          modifier = drv: pkgs.haskell.lib.addBuildTool drv pkgs.nix;
        };

        checks = {
          hlint = lint-utils.outputs.linters.${system}.hlint self;
          hpack = lint-utils.outputs.linters.${system}.hpack self;
          nixpkgs-fmt = lint-utils.outputs.linters.${system}.nixpkgs-fmt self;
          stylish-haskell = lint-utils.outputs.linters.${system}.stylish-haskell self;
        };
      };
    };
}
