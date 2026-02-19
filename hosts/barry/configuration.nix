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
      nix.storeStrategy = "aggressive";
    };
  };

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
}
