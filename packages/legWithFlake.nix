# NOTE: This should probably be an overlay, but flakelight doesn't like making
#       overlays of packages defined within the flake
{ pkgs, ... }:
pkgs.symlinkJoin {
  name = "legWithFlake";
  paths = [ pkgs.leg ];
  nativeBuildInputs = [ pkgs.makeWrapper ];
  postBuild = ''
    wrapProgram $out/bin/leg --set LEG_FLAKE "/home/logan/nixos"
  '';
}
