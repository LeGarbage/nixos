{
  pkgs,
  inputs,
  ...
}:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    inputs.home-manager.nixosModules.home-manager
    inputs.stylix.nixosModules.stylix
    inputs.self.nixosModules.desktop
    inputs.self.nixosModules.laptop
    inputs.self.nixosModules.common
  ];

  internal = {
    common.nix.storeStrategy = "normal";
  };

  networking = {
    hostName = "thermae"; # Define your hostname.
    firewall.enable = false;
  };

  # Password is required on login to unlock keyring
  security.pam.services.login.fprintAuth = false;

  # Set your time zone.
  time.timeZone = "America/Denver";

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.logan = {
    isNormalUser = true;
    description = "logan";
    extraGroups = [
      "networkmanager"
      "wheel"
      "power"
      "input"
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

  environment.systemPackages = with pkgs; [
    ffmpeg
    yt-dlp
    piper
  ];

  programs.nh.flake = "/home/logan/nixos";

  services = {
    ratbagd.enable = true;
    fprintd.enable = true;
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "26.05"; # Did you read the comment?

}
