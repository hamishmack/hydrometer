{
  description = "hydrometer: resource monitoring and job attribution for a Hydra build farm";

  # haskell.nix master: the v2 (per-component slice) builder with dependency
  # slices shared across projects (input-output-hk/haskell.nix#2606).
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils, haskellNix, ... }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-darwin" ];
      perSystem = flake-utils.lib.eachSystem supportedSystems (system:
        let
          pkgs = import nixpkgs {
            inherit system;
            inherit (haskellNix) config;
            overlays = [
              haskellNix.overlay
              (final: _prev: {
                hydrometerProject = final.haskell-nix.hix.project { src = ./.; };
              })
            ];
          };
          flake = pkgs.hydrometerProject.flake { };
          uiJs = flake.packages."javascript-unknown-ghcjs:hydrometer-ui:exe:hydrometer-ui";
          site = import ./nix/site.nix { inherit pkgs uiJs; };
        in flake // {
          legacyPackages = pkgs;
          packages = flake.packages // {
            hydrometer-site = site;
            hydrometer-agent = flake.packages."hydrometer-agent:exe:hydrometer-agent";
          } // pkgs.lib.optionalAttrs pkgs.stdenv.hostPlatform.isLinux {
            hydrometer = flake.packages."hydrometer-server:exe:hydrometer";
          };
          hydraJobs = flake.hydraJobs // { inherit site; };
        });
    in perSystem // {
      nixosModules.default = import ./nix/nixos.nix self;
      darwinModules.agent = import ./nix/darwin.nix self;
    };

  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = "true";
  };
}
