{
  description = "flake-to-gitlab-ci";
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    lint-utils.url = "git+https://gitlab.homotopic.tech/nix/lint-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    haskellNix.url = "github:input-output-hk/haskell.nix";
  };
  outputs = { self, nixpkgs, flake-utils, lint-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        deferPluginErrors = true;
        overlays = [
          haskellNix.overlay
          (final: prev: {
            flake-to-gitlab-ci =
              final.haskell-nix.project' {
                src = ./.;
                compiler-nix-name = "ghc922";
                projectFileName = "stack.yaml";
                modules = [{
                  reinstallableLibGhc = true;
                }];
              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.flake-to-gitlab-ci.flake { };
        f2gci-wrapped = pkgs.writers.writeBashBin "f2gci-wrapped" ''
           export PATH=$PATH:${pkgs.nix}
           ${flake.packages."flake-to-gitlab-ci:exe:flake-to-gitlab-ci-exe"}/bin/flake-to-gitlab-ci-exe
         '';
      in flake // {
           defaultApp = {
             type = "app";
             program = "${f2gci-wrapped}/bin/f2gci-wrapped";
           };
           defaultPackage = flake.packages."flake-to-gitlab-ci:exe:flake-to-gitlab-ci-exe";
         });
}
