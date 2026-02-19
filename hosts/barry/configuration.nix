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
    inputs.self.nixosModules.common
  ];

  internal = {
    common = {
      boot.blankTty = 60;
      nix.storeStrategy = "aggressive";
      tailscale.exitNode.enable = true;
    };
  };

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
    extraGroups = [
      "networkmanager"
      "wheel"
    ];
  };

  environment.systemPackages = [
    pkgs.stow
  ];

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "26.05"; # Did you read the comment?
}
