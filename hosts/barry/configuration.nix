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
    inputs.self.nixosModules.common
    inputs.self.nixosModules.server
  ];

  internal = {
    common = {
      boot.blankTty = 60;
      nix.storeStrategy = "aggressive";
      tailscale.exitNode.enable = true;
    };

    server.enable = true;
  };

  # Allow any user in the wheel group to import a remote configuration
  nix.settings.trusted-users = [ "@wheel" ];

  networking = {
    hostName = "barry";
    firewall.enable = false;
  };

  # Set your time zone.
  time.timeZone = "America/Denver";

  hardware = {
    enableAllFirmware = true;
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.logan = {
    isNormalUser = true;
    description = "logan";
    linger = true;
    extraGroups = [
      "networkmanager"
      "wheel"
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
  ];

  services = {
    radicale = {
      enable = true;

      settings = {
        server.hosts = [ "localhost:5232" ];
        auth = {
          type = "htpasswd";
          htpasswd_filename = "/etc/radicale/users";
          htpasswd_encryption = "sha512";
        };
      };
    };

    caddy = {
      enable = true;

      virtualHosts = {
        "${config.networking.hostName}.tadpole-escalator.ts.net:5233" = {
          extraConfig = ''
            reverse_proxy localhost:5232
          '';
        };
      };
    };

    tailscale = {
      # Needed for caddy to get TLS certs
      permitCertUid = "caddy";
    };
  };

  systemd.services.NetworkManager.restartIfChanged = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "26.05"; # Did you read the comment?
}
