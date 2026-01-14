{ pkgs, ... }:

let
  hpkgs = pkgs.haskell.packages.ghc96;
in
hpkgs.shellFor {
  packages = p: [
    pkgs.leg
  ];
  nativeBuildInputs = [
    hpkgs.haskell-language-server
    hpkgs.cabal-install
  ];
}
