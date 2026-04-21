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
  imports = [ ./boot.nix ];
  options = {
    internal.desktop.enable = lib.mkEnableOption "system desktop configuration";
    internal.desktop.remapCapslock = lib.mkEnableOption "remap capslock with keyd to be esc on press and ctrl on hold";
  };
  config = lib.mkIf cfg.enable {
    internal.desktop.boot.enable = true;

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
      keyd = lib.mkIf cfg.remapCapslock {
        enable = true;
        keyboards = {
          default = {
            ids = [ "*" ];
            settings = {
              main = {
                # Maps capslock to escape when pressed and control when held
                capslock = "overload(control, esc)";
                # Remaps the escape key to capslock
                esc = "capslock";
              };
            };
          };
        };
      };

      hypridle.enable = true;

      # For hyprpanel
      upower.enable = true;

      # For sddm
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
