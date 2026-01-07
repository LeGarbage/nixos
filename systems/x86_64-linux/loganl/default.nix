{
  pkgs,
  inputs,
  system,
  config,
  ...
}:
{
  imports = [
    ./configuration.nix
    ./hardware-configuration.nix
  ];
}
