{
  config,
  lib,
  ...
}:
let
  cfg = config.internal.common.tailscale;
in
{
  imports = [ ];
  options = {
    internal.common.tailscale.exitNode.enable = lib.mkEnableOption "using this device as an exit node";
  };
  config = lib.mkMerge [
    {
      services = {
        tailscale.enable = true;
      };

      networking.nftables.enable = true;

      systemd.services.tailscaled.serviceConfig.Environment = [
        "TS_DEBUG_FIREWALL_MODE=nftables"
      ];
    }
    (lib.mkIf cfg.exitNode.enable {
      boot.kernel.sysctl."net.ipv4.ip_forward" = 1;
      boot.kernel.sysctl."net.ipv6.conf.all.forwarding" = 1;
    })
  ];
}
