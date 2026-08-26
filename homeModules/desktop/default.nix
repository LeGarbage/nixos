{
  pkgs,
  ...
}:
{
  imports = [
    ./wallpaper.nix
    ./stylix.nix
    ./firefox.nix
    ./xdg.nix
    ./dconf.nix
  ];

  home.packages = with pkgs; [
    ghostty
    neovide
    rofi
    nautilus
    loupe
    dconf-editor
    zed-editor
    spotify
    discord
    vlc
    gnome-calendar
    planify
    trilium-desktop

    # For Hyprland
    brightnessctl
    playerctl
  ];

  programs = {
    eclipse = {
      enable = true;
      package = pkgs.eclipses.eclipse-java;
    };

    hyprshot = {
      enable = true;
      saveLocation = "$HOME/Pictures/Screenshots";
    };
  };

  services = {
    polkit-gnome.enable = true;
    hyprsunset.enable = true;
    wayle.enable = true;

    mpris-proxy.enable = true;

    syncthing.enable = true;
    kdeconnect.enable = true;

    cliphist.enable = true;
  };

  stylix.targets = {
    gtksourceview.enable = false;
    wayle.enable = false;
  };

  gtk.enable = true;

  xdg.configFile = {
    "hypr/.luarc.json".text = /* json */ ''
      {
        "workspace": {
          "library": [
            "${pkgs.hyprland}/share/hypr/stubs"
          ]
        }
      }
    '';
    "hypr/nixos/plugins.lua".text = "";
    # let
    #   hyprspace = pkgs.hyprlandPlugins.hyprspace;
    # in
    # /* lua */ ''
    #   hl.on("hyprland.start", function()
    #     hl.exec_cmd("hyprctl plugin load ${hyprspace}/lib/lib${hyprspace.pname}.so")
    #   end)
    # '';
  };
}
