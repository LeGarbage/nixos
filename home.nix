{ pkgs, config, ... }:
{
  services = {
    syncthing.enable = true;
    kdeconnect.enable = true;

    cliphist.enable = true;

    mpd = {
      enable = true;
      musicDirectory = "${config.home.homeDirectory}/Music";
      network.startWhenNeeded = true;
      extraConfig = ''
        audio_output {
            type "pipewire"
            name "PipeWire Sound Server"
        }
      '';
    };
    mpd-mpris.enable = true;

    wpaperd = {
      enable = true;
      settings = {
        eDP-1 = {
          path = pkgs.nixos-artwork.wallpapers.binary-blue;
        };
      };
    };
  };

  programs = {
    firefox = {
      enable = true;
      profiles = {
        default = {
          id = 0;
          name = "default";
          isDefault = true;
        };
        school = {
          id = 1;
          name = "school";
        };
      };
    };

    git = {
      enable = true;
      settings = {
        user = {
          name = "Logan Lessen";
          email = "loganlessen@gmail.com";
        };
        init = {
          defaultBranch = "main";
        };
      };
    };

    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    rmpc = {
      enable = true;
      config = ''
        ()
      '';
    };

    # Let Home Manager install and manage itself.
    home-manager.enable = true;
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
      };
    };
  };

  gtk = {
    enable = true;

    font = {
      name = "CommitMonoNerdFont";
    };
  };

  home = {
    # Home Manager needs a bit of information about you and the paths it should
    # manage.
    username = "logan";
    homeDirectory = "/home/logan";

    packages =
      let
        wrappedneovim =
          let
            buildInputs = with pkgs; [
              # Neovim
              wl-clipboard
              ripgrep
              tree-sitter
              # Telescope-fzf-native
              gcc
              # Snacks dashboard
              fortune
              # Subway surfers
              yt-dlp
              ffmpeg
              mpv
              # Direnv.nvim
              direnv
              # LSPs
              curl
              nodejs
              unzip
              gnumake
              cargo
              rustc
              # NOTE: nixfmt is installed here because it's broken in Mason
              nixfmt-rfc-style
            ];
          in
          pkgs.runCommand "nvim"
            {
              nativeBuildInputs = with pkgs; [ makeWrapper ];
              buildInputs = buildInputs;
            }
            ''
              mkdir -p $out/bin
              cp ${pkgs.neovim}/bin/nvim $out/bin/nvim

              wrapProgram $out/bin/nvim --prefix PATH : \
                  ${pkgs.lib.makeBinPath buildInputs}
            '';
      in
      [
        pkgs.vlc
        pkgs.starship
        wrappedneovim
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
