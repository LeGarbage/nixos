{
  pkgs,
  config,
  inputs,
  lib,
  ...
}:
{
  imports = [
    inputs.self.homeModules.desktop
    inputs.self.homeModules.common
  ];

  internal = {
    desktop = {
      enable = true;
      wallpaper.wpaperd.settings = {
        eDP-1 = {
          path = pkgs.nixos-artwork.wallpapers.binary-blue;
        };
      };
    };
    common = {
      nix.storeStrategy = "normal";
    };
  };

  services = {
    syncthing.enable = true;
    kdeconnect.enable = true;

    cliphist.enable = true;
  };

  programs = {
    firefox = {
      enable = true;
      configPath = "${config.xdg.configHome}/mozilla/firefox";
      profiles = {
        default = {
          id = 0;
          extensions.force = true;
          name = "default";
          isDefault = true;
        };
        school = {
          id = 1;
          name = "school";
        };
      };
    };
  };

  stylix = {
    icons = {
      enable = true;
      package = pkgs.papirus-icon-theme;
      dark = "Papirus";
      light = "Papirus";
    };
    targets = {
      firefox = {
        profileNames = [ "default" ];
        colorTheme.enable = true;
      };
    };
  };

  gtk = {
    enable = true;

    font = {
      name = "CommitMonoNerdFont";
    };

    gtk4.theme = null;
  };

  xdg = {
    # https://github.com/nix-community/stylix/issues/1958#issuecomment-3607824025
    configFile.kdeglobals.source =
      let
        themePackage = builtins.head (
          builtins.filter (
            p: builtins.match ".*stylix-kde-theme.*" (builtins.baseNameOf p) != null
          ) config.home.packages
        );
        colorSchemeSlug = lib.concatStrings (
          lib.filter lib.isString (builtins.split "[^a-zA-Z]" config.lib.stylix.colors.scheme)
        );
      in
      "${themePackage}/share/color-schemes/${colorSchemeSlug}.colors";

    userDirs = {
      enable = true;
      setSessionVariables = false;
      download = "${config.home.homeDirectory}/Downloads";
      music = "${config.home.homeDirectory}/Music";
      pictures = "${config.home.homeDirectory}/Pictures";
      extraConfig = {
        SCREENSHOTS = "${config.home.homeDirectory}/Pictures/Screenshots";
      };
    };

    mimeApps = {
      enable = true;
      defaultApplications = {
        "text/plain" = "neovide.desktop";
        "text/x-script.python" = "neovide.desktop";
        "application/x-shellscript" = "neovide.desktop";
        "application/json" = "neovide.desktop";
        "application/xml" = "neovide.desktop";
        "text/xml" = "neovide.desktop";
        "text/x-c" = "neovide.desktop";
        "text/x-c++" = "neovide.desktop";
        "text/x-java" = "neovide.desktop";
        "text/x-rust" = "neovide.desktop";
        "text/x-go" = "neovide.desktop";
        "text/markdown" = "neovide.desktop";
      };
    };
  };

  home = {
    # Home Manager needs a bit of information about you and the paths it should
    # manage.
    username = "logan";
    homeDirectory = "/home/logan";

    packages = with pkgs; [
      vlc
      starship
      wrappedNeovim
      kdePackages.plasma-integration
      kdePackages.breeze
      vscode-fhs
      obsidian
    ];

    # This value determines the Home Manager release that your configuration is
    # compatible with. This helps avoid breakage when a new Home Manager release
    # introduces backwards incompatible changes.
    #
    # You should not change this value, even if you update Home Manager. If you do
    # want to update the value, then make sure to first check the Home Manager
    # release notes.
    stateVersion = "25.05"; # Please read the comment before changing.

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

  systemd.user.targets.hyprland-session = {
    Unit = {
      Description = "Hyprland compositor session";
      Documentation = [ "man:systemd.special(7)" ];
      BindsTo = [ "graphical-session.target" ];
      Wants = [ "graphical-session-pre.target" ];
      After = [ "graphical-session-pre.target" ];
    };
  };
}
