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

  config = {
    home.packages = with pkgs; [
      linux-wallpaperengine
    ];

    services.wpaperd = {
      enable = true;
      settings = cfg.wpaperd.settings;
    };
  };
}
