{
  config,
  pkgs,
  lib,
  inputs,
  ...
}:
{
  imports = [
    inputs.self.homeModules.common
  ];

  internal = {
    common = {
      nix.storeStrategy = "aggressive";
    };
  };

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
          obsidianPath = "${config.home.homeDirectory}/obsidian/main";
          obsidianOpts = {
            pruneOpts = [
              "--keep-daily 7"
              "--keep-weekly 4"
              "--keep-monthly 12"
            ];
            exclude = [
              ".stfolder"
              ".trash"
              ".direnv"
              ".rumdl_cache"
            ];
            paths = [ obsidianPath ];
            timerConfig = null;
          };
        in
        {
          obsidian-main-local = {
            initialize = true;
            repository = "/srv/backups/obsidian";
            passwordCommand = "${lib.getExe pkgs.pass} restic/obsidian-local";
          }
          // obsidianOpts;
          obsidian-main-remote = {
            initialize = true;
            repository = "rclone:drive:backups/obsidian";
            passwordCommand = "${lib.getExe pkgs.pass} restic/obsidian-remote";
          }
          // obsidianOpts;
        };
    };

    gpg-agent = {
      enable = true;
      pinentry = {
        package = pkgs.pinentry-gnome3;
      };
    };
  };

  systemd.user = {
    services = {
      syncthing.Install.WantedBy = [ "multi-user.target" ];

      restic-backups-obsidian-main-format = {
        Unit = {
          Wants = [
            "restic-backups-obsidian-main-local.service"
            "restic-backups-obsidian-main-remote.service"
          ];
        };

        Service = {
          Type = "oneshot";

          ExecStart = "${pkgs.nix}/bin/nix fmt";
          WorkingDirectory = "${config.home.homeDirectory}/obsidian/main";
        };
      };

      restic-backups-obsidian-main-local = {
        Unit = {
          After = [ "restic-backups-obsidian-main-format.service" ];
        };
      };

      restic-backups-obsidian-main-remote = {
        Unit = {
          After = [ "restic-backups-obsidian-main-format.service" ];
        };
      };
    };

    timers = {
      restic-backups-obsidian-main-format = {
        Install.WantedBy = [ "timers.target" ];

        Timer = {
          OnCalendar = "daily";
          Persistent = true;
        };
      };
    };
  };

  home = {
    # Home Manager needs a bit of information about you and the paths it should
    # manage.
    username = "logan";
    homeDirectory = "/home/logan";

    packages = with pkgs; [
      starship
      wrappedNeovim
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

    # The home.packages option allows you to install Nix packages into your
    # environment.

    # Home Manager is pretty good at managing dotfiles. The primary way to manage
    # plain files is through 'home.file'.
    file = {
      # # Building this configuration will create a copy of 'dotfiles/screenrc' in
      # # the Nix store. Activating the configuration will then make '~/.screenrc' a
      # # symlink to the Nix store copy.
      # ".screenrc".source = dotfiles/screenrc;

      # # You can also set the file content immediately.
      # ".gradle/gradle.properties".text = ''
      #   org.gradle.console=verbose
      #   org.gradle.daemon.idletimeout=3600000
      # '';
    };

    # Home Manager can also manage your environment variables through
    # 'home.sessionVariables'. These will be explicitly sourced when using a
    # shell provided by Home Manager. If you don't want to manage your shell
    # through Home Manager then you have to manually source 'hm-session-vars.sh'
    # located at either
    #
    #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
    #
    # or
    #
    #  ~/.local/state/nix/profiles/profile/etc/profile.d/hm-session-vars.sh
    #
    # or
    #
    #  /etc/profiles/per-user/logan/etc/profile.d/hm-session-vars.sh
    #
    sessionVariables = {
      # EDITOR = "emacs";
    };
  };
}
