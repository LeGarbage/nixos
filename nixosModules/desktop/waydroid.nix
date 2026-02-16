{
  lib,
  config,
  ...
}:
let
  cfg = config.internal.desktop.waydroid;
in
{
  options = {
    internal.desktop.waydroid.enable = lib.mkEnableOption "waydroid";
  };
  config = lib.mkIf cfg.enable {
    networking.nftables.enable = true;
    virtualisation.waydroid.enable = true;
  };
}
