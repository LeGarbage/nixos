{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.internal.common.boot;
in
{
  imports = [ ];
  options = {
    internal.common.boot.blankTty = lib.mkOption {
      description = ''
        Number passed to the `consoleblank` linux boot parameter,
        which powers off the tty after XX seconds.

        Set to 0 (default) to disable
      '';
      type = lib.types.ints.unsigned;
      default = 0;
    };
  };
  config = lib.mkMerge [
    {
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
    }
    (lib.mkIf (cfg.blankTty != 0) {
      boot.kernelParams = [ "consoleblank=${builtins.toString cfg.blankTty}" ];
    })
  ];
}
