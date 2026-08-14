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
    desktop.wallpaper.wpaperd.settings = pkgs.fetchurl {
      url = "https://raw.githubusercontent.com/Narmis-E/onedark-wallpapers/6f084e27d7a407be5c73a9fc88a5644408b74dca/minimal/od_gargantua.png";
      hash = "sha256-DhsHDfIBC5R9SZx28iuwqwhl0U3dpGYJYrpzByY4HPM=";
    };
  };

  home = {
    # Home Manager needs a bit of information about you and the paths it should
    # manage.
    username = "logan";
    homeDirectory = "/home/logan";

    # This value determines the Home Manager release that your configuration is
    # compatible with. This helps avoid breakage when a new Home Manager release
    # introduces backwards incompatible changes.
    #
    # You should not change this value, even if you update Home Manager. If you do
    # want to update the value, then make sure to first check the Home Manager
    # release notes.
    stateVersion = "26.11"; # Please read the comment before changing.
  };
}
