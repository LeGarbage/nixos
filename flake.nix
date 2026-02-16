{
  description = "Nixos config flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flakelight.url = "github:nix-community/flakelight";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    stylix = {
      url = "github:nix-community/stylix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { flakelight, ... }@inputs:
    # Use flakelight to manage the flake. Each directory in nixDir corresponds
    # to an output of the flake, with each file/subdirectory being an attribute
    # of said output
    flakelight ./. {
      inherit inputs;
      nixDir = ./.;
      # The "hosts" directory can be used in place of nixosConfigurations
      nixDirAliases.nixosConfigurations = [ "hosts" ];
    };
}
