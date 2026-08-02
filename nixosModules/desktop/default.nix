{
  pkgs,
  ...
}:
{
  imports = [
    ./boot-splash.nix
    ./display-manager.nix
  ];
  config = {
    environment.systemPackages = with pkgs; [
      wl-clipboard
      gnome-online-accounts-gtk
    ];

    programs = {
      hyprland = {
        enable = true;
        withUWSM = true;
      };

      hyprlock.enable = true;

      seahorse.enable = true;
      dconf.enable = true;

      localsend.enable = true;
    };

    services = {
      keyd = {
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

      gnome = {
        gnome-keyring.enable = true;
        evolution-data-server.enable = true;
        gnome-online-accounts.enable = true;
      };
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
