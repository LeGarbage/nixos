{
  pkgs,
  lib,
  inputs,
  ...
}:
{
  imports = [
    inputs.self.homeModules.common
  ];

  programs = {
    password-store.enable = true;
    gpg.enable = true;
  };

  services = {
    syncthing.enable = true;

    restic = {
      enable = true;
      backups =
        let
          pruneOpts = [
            "--keep-daily 7"
            "--keep-weekly 4"
            "--keep-monthly 12"
          ];
        in
        {
          radicale-remote = {
            inherit pruneOpts;
            initialize = true;
            repository = "rclone:drive:backups/radicale";
            passwordCommand = "${lib.getExe pkgs.pass} restic/radicale-remote";
            exclude = [
              ".Radicale.cache"
              ".Radicale.lock"
              ".Radicale.tmp-*"
            ];
            paths = [ "/var/lib/radicale/collections" ];
          };
          trilium-remote = {
            inherit pruneOpts;
            repository = "rclone:drive:backups/trilium";
            passwordCommand = "${lib.getExe pkgs.pass} restic/trilium-remote";
            paths = [ "/var/lib/trilium/backup/backup-daily.db" ];
          };
        };
    };

    gpg-agent = {
      enable = true;
      pinentry = {
        package = pkgs.pinentry-gnome3;
      };
    };
  };

  home = {
    # Home Manager needs a bit of information about you and the paths it should
    # manage.
    username = "logan";
    homeDirectory = "/home/logan";

    packages = with pkgs; [
      gcr
      # TODO: Configure with home manager once rclone's configuration gets fixed
      #       https://discourse.nixos.org/t/programs-modifying-config-files-created-by-home-manager/42878
      #       https://github.com/rclone/rclone/issues/3655
      rclone
    ];

    # This value determines the Home Manager release that your configuration is
    # compatible with. This helps avoid breakage when a new Home Manager release
    # introduces backwards incompatible changes.
    #
    # You should not change this value, even if you update Home Manager. If you do
    # want to update the value, then make sure to first check the Home Manager
    # release notes.
    stateVersion = "26.05"; # Please read the comment before changing.
  };
}
