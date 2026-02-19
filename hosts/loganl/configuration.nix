{
  pkgs,
  config,
  inputs,
  ...
}:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    inputs.home-manager.nixosModules.home-manager
    inputs.self.nixosModules.desktop
    inputs.self.nixosModules.laptop
    inputs.self.nixosModules.common
  ];

  internal = {
    desktop.enable = true;
    laptop.enable = true;
    common = {
      nix.storeStrategy = "normal";
    };
  };

  networking = {
    hostName = "loganl"; # Define your hostname.
    firewall.enable = false;
  };

  # Set your time zone.
  time.timeZone = "America/Denver";

  hardware = {
    bluetooth.enable = true;
    enableAllFirmware = true;
  };

  stylix = {
    enable = true;
    base16Scheme = "${pkgs.base16-schemes}/share/themes/onedark.yaml";
    polarity = "dark";

    cursor = {
      package = pkgs.simp1e-cursors;
      name = "Simp1e-Dark";
      size = 24;
    };

    fonts = {
      monospace = {
        package = pkgs.nerd-fonts.commit-mono;
        name = "CommitMonoNerdFont";
      };
      serif = config.stylix.fonts.monospace;
      sansSerif = config.stylix.fonts.monospace;
      emoji = config.stylix.fonts.monospace;
    };
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.logan = {
    isNormalUser = true;
    description = "logan";
    extraGroups = [
      "networkmanager"
      "wheel"
      "power"
    ];
  };

  home-manager = {
    extraSpecialArgs = { inherit inputs; };
    backupFileExtension = "bak";
    useGlobalPkgs = true;
    useUserPackages = true;
    users = {
      "logan" = import ./home.nix;
    };
  };

  environment.systemPackages = [
    pkgs.stow
    pkgs.ffmpeg
    pkgs.yt-dlp
    pkgs.piper
  ];

  programs = {
    nix-ld = {
      enable = true;
      libraries = with pkgs; [
        # Add any missing dynamic libraries for unpackaged programs
        # here, NOT in environment.systemPackages
        glibc
      ];
    };

    steam.enable = true;
  };

  services = {
    ratbagd.enable = true;
  };

  systemd = {
    packages = with pkgs; [ ghostty ];
    user.services."app-com.mitchellh.ghostty".wantedBy = [ "graphical-session.target" ];
  };

  fonts = {
    packages = with pkgs; [ nerd-fonts.commit-mono ];
    fontconfig = {
      defaultFonts.monospace = [ "CommitMonoNerdFont" ];
      defaultFonts.sansSerif = [ "CommitMonoNerdFont" ];
      defaultFonts.serif = [ "CommitMonoNerdFont" ];
    };
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "25.05"; # Did you read the comment?

}
