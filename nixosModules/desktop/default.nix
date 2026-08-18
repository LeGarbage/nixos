{
  pkgs,
  ...
}:
{
  imports = [
    ./boot-splash.nix
    ./display-manager.nix
    ./stylix.nix
    ./mission-center.nix
  ];

  environment.systemPackages = with pkgs; [
    wl-clipboard
    gnome-online-accounts-gtk
    libreoffice
  ];

  programs = {
    hyprland = {
      enable = true;
      withUWSM = true;
    };

    hyprlock.enable = true;

    seahorse.enable = true;
    dconf.enable = true;
    gnome-disks.enable = true;

    localsend.enable = true;

    steam.enable = true;
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
    user.services = {
      "app-com.mitchellh.ghostty" = {
        wantedBy = [ "graphical-session.target" ];
        enableDefaultPath = false;
      };
      hypridle.path = [ pkgs.brightnessctl ];
    };
  };

  hardware = {
    bluetooth = {
      enable = true;
      powerOnBoot = true;
      settings = {
        General = {
          Experimental = true;
        };
      };
    };
  };
}
