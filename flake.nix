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
      in flake // {
           defaultPackage = flake.packages."flake-to-gitlab-ci:exe:flake-to-gitlab-ci-exe";
         });
}
