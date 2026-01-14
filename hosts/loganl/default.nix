{ inputs, ... }:
{
  modules = [
    ./configuration.nix
    inputs.home-manager.nixosModules.default
    inputs.stylix.nixosModules.stylix
  ];
}
