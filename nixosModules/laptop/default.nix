{
  ...
}:
{
  config = {
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
