let
  compilerVersion = "ghc864";
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  ghcide = (import sources.ghcide-nix {})."ghcide-${compilerVersion}";
in

with pkgs;

let
  hpkgs = haskell.packages."${compilerVersion}";
  btools = [
    hpkgs.hpack
    hpkgs.cabal-install
    hpkgs.ghcid
    hpkgs.hoogle
    hpkgs.stylish-cabal
    ghcide
  ];
  modifier = drv: haskell.lib.addBuildTools drv btools;
in

hpkgs.developPackage { root = ./.; inherit modifier; returnShellEnv = false; }
