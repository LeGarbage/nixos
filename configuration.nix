{
  config,
  pkgs,
  inputs,
  ...
}:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    inputs.home-manager.nixosModules.default
  ];

  boot = {
    # Bootloader.
    loader = {
      efi.canTouchEfiVariables = true;
      efi.efiSysMountPoint = "/boot";
      grub = {
        enable = true;
        efiSupport = true;
        device = "nodev";
        configurationLimit = 10;
        extraEntries = ''
          menuentry "System shutdown" {
              echo "System shutting down..."
              halt
          }


          if [ ''${grub_platform} == "efi" ]; then
              menuentry 'UEFI Firmware Settings' --id 'uefi-firmware' {
                  fwsetup
              }
          fi
        '';
      };
    };

    kernelPackages = pkgs.linuxPackages_latest;
  };

  networking = {

    hostName = "loganl"; # Define your hostname.

    networkmanager.enable = true;

    firewall.enable = false;
  };

  # Set your time zone.
  time.timeZone = "America/Denver";

  hardware.bluetooth.enable = true;

  # powerManagement.powertop.enable = true;

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
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
    users = {
      "logan" = import ./home.nix;
    };
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;
  nix = {
    settings = {
      # Use flakes
      experimental-features = [
        "nix-command"
        "flakes"
      ];
    };
    optimise = {
      automatic = true;
      dates = "weekly";
    };
    gc = {
      automatic = true;
      options = "--delete-older-than 14d";
      dates = "weekly";
    };
  };

  environment.systemPackages =
    let
      rofi = pkgs.rofi.override { plugins = [ pkgs.rofi-calc ]; };
      leg = pkgs.symlinkJoin {
        name = "leg";
        paths = [ inputs.self.packages.${pkgs.stdenv.hostPlatform.system}.leg ];
        nativeBuildInputs = [ pkgs.makeWrapper ];
        postBuild = ''
          wrapProgram $out/bin/leg --set LEG_FLAKE "/home/logan/nixos"
        '';
      };
      # leg = inputs.self.packages.${pkgs.stdenv.hostPlatform.system}.leg;
    in
    [
      leg
      rofi
      pkgs.ghostty
      pkgs.git
      pkgs.stow
      pkgs.hyprpanel
      pkgs.yazi
      pkgs.neovide
      pkgs.brightnessctl
      pkgs.linux-wallpaperengine
      pkgs.acpid
      pkgs.socat
      pkgs.jq
      pkgs.wl-clipboard
      pkgs.nodejs
      pkgs.ffmpeg
      pkgs.yt-dlp
      pkgs.where-is-my-sddm-theme
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

    hyprland.enable = true;
    hyprlock.enable = true;

    neovim = {
      enable = true;
      defaultEditor = true;
    };

    bash = {
      enable = true;
      blesh.enable = true;
    };

    steam.enable = true;

    firefox.enable = true;
  };

  services = {
    hypridle.enable = true;

    upower.enable = true;
    acpid.enable = true;

    xserver.enable = true;

    displayManager = {
      enable = true;
      sddm = {
        enable = true;
        # wayland.enable = true;
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

    logind = {
      settings.Login = {
        HandleLidSwitchDocked = "suspend";
      };
    };

    tlp.enable = true;

    # Enable the OpenSSH daemon.
    openssh.enable = true;

    # Tailscale
    tailscale.enable = true;
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
