{
  config,
  pkgs,
  ...
}:
let
  cfg = config.internal.common.boot;
in
{
  imports = [ ];
  options = {
  };
  config = {
    boot = {
      # Bootloader.
      loader = {
        efi.canTouchEfiVariables = true;
        efi.efiSysMountPoint = "/boot";
        grub = {
          enable = true;
          efiSupport = true;
          device = "nodev";
          configurationLimit = 10;
          extraEntries = ''
            menuentry "System shutdown" {
                echo "System shutting down..."
                halt
            }


            if [ ''${grub_platform} == "efi" ]; then
                menuentry 'UEFI Firmware Settings' --id 'uefi-firmware' {
                    fwsetup
                }
            fi
          '';
        };
      };

      kernelPackages = pkgs.linuxPackages_latest;
    };
  };
}
