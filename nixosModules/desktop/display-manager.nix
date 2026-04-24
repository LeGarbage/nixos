{
  config,
  pkgs,
  ...
}:
let
  cfg = config.internal.desktop.displayManager;
in
{
  imports = [ ];
  options = { };
  config = {
    services = {
      greetd = {
        enable = true;
        settings = {
          default_session = {
            command =
              let
                hyprConfig = pkgs.writeText "greetd-hypr-config" /* hyprlang */ ''
                  exec-once = ${pkgs.regreet}/bin/regreet; ${pkgs.hyprland}/bin/hyprctl dispatch exit
                  exec-once = ${pkgs.hyprland}/bin/hyprctl setcursor Simp1e-Dark 24
                  misc {
                      disable_hyprland_logo = true
                      disable_splash_rendering = true
                      disable_hyprland_guiutils_check = true
                  }

                  monitor=,preferred,auto-left,1, mirror, eDP-1
                '';
              in
              "${pkgs.dbus}/bin/dbus-run-session ${pkgs.hyprland}/bin/start-hyprland -- -c ${hyprConfig}";
          };
        };
      };
    };

    programs.regreet = {
      enable = true;
    };
  };
}
