{
  ...
}:
{
  config = {
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
        # It's still possible to open the bootloader list by holding escape
        timeout = 0;
        grub.timeoutStyle = "hidden";
      };
    };
  };
}
