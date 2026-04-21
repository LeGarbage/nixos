{
  config,
  lib,
  ...
}:
let
  cfg = config.internal.desktop.boot;
in
{
  imports = [ ];
  options = {
    internal.desktop.boot.enable = lib.mkEnableOption "plymouth boot splash";
  };
  config = lib.mkIf cfg.enable {
    boot = {
      plymouth = {
        enable = true;
      };

      # Enable "Silent boot"
      consoleLogLevel = 3;
      initrd.verbose = false;
      kernelParams = [
        "quiet"
        "udev.log_level=3"
        "systemd.show_status=auto"
      ];

      loader = {
        # Hide the OS choice for bootloaders.
        # It's still possible to open the bootloader list by pressing any key
        # It will just not appear on screen unless a key is pressed
        timeout = 0;
        grub.timeoutStyle = "hidden";
      };
    };
  };
}
