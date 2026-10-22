{ pkgs, ... }:
let
  # Patch a hackage tarball and hand the PATCHED source to the solver as a
  # local `packages:` entry, so plan-nix and the slice build see the same code.
  patchedHackage = name: version: patch: pkgs.applyPatches {
    name = "${name}-${version}-patched";
    src = pkgs.haskell-nix.hackageTarball { inherit name version; };
    patches = [ patch ];
  };
  # reflex-dom-core selects its jsffi-flavoured `src-ghcjs` on arch(javascript),
  # which only typechecks against ghcjs-dom's jsffi flavour.  The UI is written
  # against the jsaddle flavour, so keep `src-ghcjs` for legacy GHCJS only.
  reflexDomCoreJsaddleJs = pkgs.lib.optionalString pkgs.stdenv.hostPlatform.isGhcjs ''
    packages: ${patchedHackage "reflex-dom-core" "0.8.1.4" ./patches/reflex-dom-core-js-jsaddle.patch}
  '';
in {
  name = "hydrometer";
  compiler-nix-name = "ghc914-sh";
  # haskell.nix's cabal-project.nix sets builderVersion too; mkForce avoids a
  # "conflicting definition values" error.
  builderVersion = pkgs.lib.mkForce 2;
  cabalProjectLocal = reflexDomCoreJsaddleJs;
  # The UI is the only component built for the browser.
  crossPlatforms = p: [ p.ghcjs ];
  modules = [ ({ lib, ... }: {
    packages.reflex.components.tests.hlint.buildable = lib.mkForce false;
    packages.reflex.components.tests.RequesterT.buildable = lib.mkForce false;
    packages.reflex.components.tests.QueryT.buildable = lib.mkForce false;
    packages.reflex.components.tests.EventWriterT.buildable = lib.mkForce false;
    packages.reflex.components.tests.DebugCycles.buildable = lib.mkForce false;
  }) ];
  shell = {
    crossPlatforms = _: [ ];
    tools.cabal = pkgs.pkgsBuildBuild.haskell-nix.v2-cabal-install;
    buildInputs = [ pkgs.postgresql ];
  };
}
