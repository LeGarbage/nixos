{ pkgs, ... }:

let
  hpkgs = pkgs.haskell.packages.ghc96;
in
hpkgs.developPackage { root = ./.; }
