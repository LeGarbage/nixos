{
  config,
  ...
}:
let
  cfg = config.internal.common;
in
{
  imports = [
    ./boot.nix
    ./nix.nix
  ];
  options = {
  };
  config = {
    programs = {
      nano.enable = false;
      neovim = {
        enable = true;
        defaultEditor = true;
      };

      bash = {
        enable = true;
        blesh.enable = true;
      };

      git.enable = true;
    };

    services = {
      tailscale.enable = true;
    };

    # Select internationalisation properties.
    i18n.defaultLocale = "en_US.UTF-8";

    i18n.extraLocaleSettings = {
      LC_ADDRESS = "en_US.UTF-8";
      LC_IDENTIFICATION = "en_US.UTF-8";
      LC_MEASUREMENT = "en_US.UTF-8";
      LC_MONETARY = "en_US.UTF-8";
      LC_NAME = "en_US.UTF-8";
      LC_NUMERIC = "en_US.UTF-8";
      LC_PAPER = "en_US.UTF-8";
      LC_TELEPHONE = "en_US.UTF-8";
      LC_TIME = "en_US.UTF-8";
    };
  };
}
