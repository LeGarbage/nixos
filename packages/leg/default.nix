{ pkgs, ... }:

let
  hpkgs = pkgs.haskell.packages.ghc96;
in
(hpkgs.developPackage { root = ./.; }).overrideAttrs (old: {
  postInstall = ''
    mkdir -p $out/share/bash-completion/completions/
    $out/bin/leg --bash-completion-script $out/bin/leg > \
      $out/share/bash-completion/completions/leg
  '';
})
