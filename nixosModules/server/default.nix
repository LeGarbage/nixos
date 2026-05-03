{
  config,
  lib,
  ...
}:
let
  cfg = config.internal.server;
in
{
  imports = [
    ./ssh.nix
  ];
  options = {
    internal.server.enable = lib.mkEnableOption "server configuration";
  };
  config = lib.mkIf cfg.enable {
    internal.server.ssh.enable = lib.mkDefault true;
  };
}
