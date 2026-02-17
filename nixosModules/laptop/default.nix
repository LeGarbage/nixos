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
        settings = {
          CPU_BOOST_ON_AC = 1;
          CPU_BOOST_ON_BAT = 0;
          CPU_SCALING_GOVERNOR_ON_AC = "performance";
          CPU_SCALING_GOVERNOR_ON_BAT = "powersave";
          CPU_ENERGY_PERF_POLICY_ON_BAT = "power";
          PLATFORM_PROFILE_ON_BAT = "low-power";
        };
      };
      thermald.enable = true;
    };
  };
}
