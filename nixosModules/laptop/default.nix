{
  config,
  lib,
  ...
}:
let
  cfg = config.internal.desktop;
in
{
  imports = [ ];
  options = {
    internal.laptop.enable = lib.mkEnableOption "laptop config";
  };
  config = lib.mkIf cfg.enable {
    services = {
      logind = {
        settings.Login = {
          HandleLidSwitchDocked = "suspend";
        };
      };

      tlp = {
        enable = true;
        pd.enable = true;
        settings = {
          TLP_AUTO_SWITCH = 1;
        };
      };
      thermald.enable = true;
    };
  };
}
