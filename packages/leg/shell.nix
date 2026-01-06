{ pkgs, ... }:

let
  hpkgs = pkgs.haskell.packages.ghc96;
in
hpkgs.shellFor {
  packages = p: [
    (import ./. { inherit pkgs; })
  ];
  nativeBuildInputs = [
    hpkgs.haskell-language-server
    hpkgs.cabal-install
  ];
}
