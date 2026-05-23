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
      neovide
      rofi
      nautilus
      loupe
      mission-center
      zed-editor
      obsidian
      spotify
      discord
      vlc
      gnome-calendar
      planify

      # For Hyprland
      brightnessctl
      playerctl
    ];

    services = {
      hyprpolkitagent.enable = true;
      hyprsunset.enable = true;
      wayle.enable = true;
    };

    stylix.targets.gtksourceview.enable = false;

    xdg.configFile."hypr/.luarc.json".text = /* json */ ''
      {
        "workspace": {
          "library": [
            "${pkgs.hyprland}/share/hypr/stubs"
          ]
        }
      }
    '';

    internal.desktop.wallpaper.enable = lib.mkDefault true;
  };
}
