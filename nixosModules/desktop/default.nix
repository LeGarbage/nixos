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
  imports = [
    ./boot-splash.nix
    ./display-manager.nix
  ];
  options = {
    internal.desktop.enable = lib.mkEnableOption "system desktop configuration";
    internal.desktop.remapCapslock = lib.mkEnableOption "remap capslock with keyd to be esc on press and ctrl on hold";
  };
  config = lib.mkIf cfg.enable {
    internal.desktop.bootSplash.enable = true;

    environment.systemPackages = with pkgs; [
      wl-clipboard
    ];

    programs = {
      hyprland = {
        enable = true;
        withUWSM = true;
      };

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
              };
            };
          };
        };
      };

      hypridle.enable = true;

      # For wayle
      upower.enable = true;

      # For nautilus
      gvfs.enable = true;
    };

    security.polkit.enable = true;

    systemd = {
      packages = with pkgs; [ ghostty ];
      user.services."app-com.mitchellh.ghostty" = {
        wantedBy = [ "graphical-session.target" ];
        enableDefaultPath = false;
      };
    };
  };
}
