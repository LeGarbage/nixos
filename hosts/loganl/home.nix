{
  pkgs,
  inputs,
  ...
}:
{
  imports = [
    inputs.self.homeModules.desktop
    inputs.self.homeModules.common
  ];

  internal = {
    desktop.wallpaper.wpaperd.settings = {
      eDP-1 = {
        path = pkgs.fetchurl {
          url = "https://raw.githubusercontent.com/Narmis-E/onedark-wallpapers/6f084e27d7a407be5c73a9fc88a5644408b74dca/minimal/od_planets.png";
          hash = "sha256-x9Btp5f+i5QAQPJHDGEJHcRDvBIKDmoG65DmlhTqoOQ=";
        };
      };
    };
  };

  home = {
    # Home Manager needs a bit of information about you and the paths it should
    # manage.
    username = "logan";
    homeDirectory = "/home/logan";

    packages = with pkgs; [
      kdePackages.plasma-integration
      kdePackages.breeze

      prismlauncher
    ];

    # This value determines the Home Manager release that your configuration is
    # compatible with. This helps avoid breakage when a new Home Manager release
    # introduces backwards incompatible changes.
    #
    # You should not change this value, even if you update Home Manager. If you do
    # want to update the value, then make sure to first check the Home Manager
    # release notes.
    stateVersion = "25.05"; # Please read the comment before changing.
  };
}
