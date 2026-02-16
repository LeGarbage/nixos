{
  pkgs,
  lib,
  config,
  ...
}:
let
  cfg = config.internal.desktop.wallpaper;
in
{
  options = {
    internal.desktop.wallpaper.enable = lib.mkEnableOption "wallpapers using linux-wallpaperengine and wpaperd";

    internal.desktop.wallpaper.wpaperd.settings = lib.mkOption {
      type = (pkgs.formats.toml { }).type;
      default = { };
      example = lib.literalExpression ''
        {
          eDP-1 = {
            path = "/home/foo/Pictures/Wallpaper";
            apply-shadow = true;
          };
          DP-2 = {
            path = "/home/foo/Pictures/Anime";
            sorting = "descending";
          };
        }
      '';
      description = ''
        Configuration passed to services.wpaperd.settings.
        See <https://nix-community.github.io/home-manager/options.xhtml#opt-services.wpaperd.settings> for more details.
      '';
    };
  };
  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      linux-wallpaperengine
      socat
      jq
      ghostty
      hyprpanel
      yazi
      neovide
      brightnessctl
    ];

    programs.hyprshot = {
      enable = true;
      # FIX: This env var is not set
      saveLocation = "$HOME/Pictures/Screenshots";
    };

    services.wpaperd = {
      enable = true;
      settings = cfg.wpaperd.settings;
    };
  };
}
