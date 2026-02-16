{
  pkgs,
  lib,
  config,
  ...
}:
let
  cfg = config.internal.desktop;
in
{
  imports = [ ./wallpaper.nix ];
  options = {
    internal.desktop.enable = lib.mkEnableOption "user desktop configuration";
  };
  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      ghostty
      yazi
      neovide
      hyprpanel

      # Hyprpanel
      brightnessctl
    ];

    internal.desktop.wallpaper.enable = lib.mkDefault true;
  };
}
