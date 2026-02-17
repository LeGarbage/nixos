{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.internal.desktop;
in
{
  imports = [ ];
  options = {
    internal.desktop.enable = lib.mkEnableOption "system desktop configuration";
  };
  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      where-is-my-sddm-theme
      wl-clipboard
    ];

    programs = {
      hyprland.enable = true;
      hyprlock.enable = true;

      firefox.enable = true;
    };

    services = {
      hypridle.enable = true;

      # For hyprpanel
      upower.enable = true;

      xserver.enable = true;
      displayManager = {
        enable = true;
        sddm = {
          enable = true;
          theme = "${
            pkgs.where-is-my-sddm-theme.override {
              themeConfig.General = {
                passwordCursorColor = "#ffffff";
                passwordInputWidth = 0.75;
              };
            }
          }/share/sddm/themes/where_is_my_sddm_theme";
          extraPackages = [ pkgs.where-is-my-sddm-theme ];
        };
      };
    };
  };
}
