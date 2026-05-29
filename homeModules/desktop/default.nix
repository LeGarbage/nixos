{
  pkgs,
  lib,
  config,
  ...
}:
let
  cfg = config.internal.desktop;
in
{
  imports = [ ./wallpaper.nix ];
  options = {
    internal.desktop.enable = lib.mkEnableOption "user desktop configuration";
  };
  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      ghostty
      neovide
      rofi
      nautilus
      loupe
      mission-center
      zed-editor
      obsidian
      spotify
      discord
      vlc
      gnome-calendar
      planify

      # For Hyprland
      brightnessctl
      playerctl
    ];

    programs = {
      firefox = {
        enable = true;
        configPath = "${config.xdg.configHome}/mozilla/firefox";
        policies = {
          ExtensionSettings = {
            "uBlock0@raymondhill.net" = {
              installation_mode = "force_installed";
              install_url = "https://addons.mozilla.org/firefox/downloads/latest/ublock-origin/latest.xpi";
              default_area = "navbar";
              private_browsing = true;
            };
            # Vimium
            "{d7742d87-e61d-4b78-b8a1-b469842139fa}" = {
              installation_mode = "force_installed";
              install_url = "https://addons.mozilla.org/firefox/downloads/latest/vimium-ff/latest.xpi";
              default_area = "navbar";
            };
          };
          DisableTelemetry = true;
          FirefoxHome = {
            "Search" = true;
            "TopSites" = false;
            "SponsoredTopSites" = false;
            "Highlights" = false;
            "Pocket" = false;
            "Stories" = false;
            "SponsoredPocket" = false;
            "SponsoredStories" = false;
            "Snippets" = false;
            "Locked" = true;
          };
          NoDefaultBookmarks = true;
          SkipTermsOfUse = true;
          EnableTrackingProtection.Category = "strict";
          SearchEngines.Remove = [
            "Amazon.com"
            "Bing"
            "eBay"
            "Perplexity"
            "Wikipedia (en)"
          ];
        };
        profiles = {
          default = {
            id = 0;
            extensions.force = true;
            name = "default";
            isDefault = true;
            settings = {
              "browser.ai.control.default" = "blocked";
              "browser.urlbar.suggest.bookmark" = false;
              "browser.urlbar.suggest.engines" = false;
              "browser.urlbar.suggest.sponsored" = false;
              "sidebar.main.tools" = "syncedtabs,history,passwords";
              "sidebar.revamp" = true;
              "sidebar.verticalTabs" = true;
              "sidebar.expandOnHover" = false;
              "browser.toolbars.bookmarks.visibility" = "never";
              "browser.newtabpage.activity-stream.widgets.weather.enabled" = false;
              "privacy.globalprivacycontrol.enabled" = true;
              # Open previous tabs
              "browser.startup.page" = 3;
            };
            search = {
              force = true;
              default = "ddg";
              engines = {
                nixpkgs = {
                  name = "Nixpkgs";
                  urls = [
                    {
                      template = "https://search.nixos.org/packages";
                      params = [
                        {
                          name = "channel";
                          value = "unstable";
                        }
                        {
                          name = "query";
                          value = "{searchTerms}";
                        }
                      ];
                    }
                  ];
                  definedAliases = [
                    "@n"
                    "@nix"
                  ];
                };
                nix-modules = {
                  name = "Nix Modules";
                  urls = [
                    {
                      template = "https://search.nixos.org/options";
                      params = [
                        {
                          name = "channel";
                          value = "unstable";
                        }
                        {
                          name = "query";
                          value = "{searchTerms}";
                        }
                      ];
                    }
                  ];
                  definedAliases = [
                    "@o"
                    "@opt"
                  ];
                };
                nix-wiki = {
                  name = "NixOS Wiki";
                  urls = [
                    {
                      template = "https://wiki.nixos.org/w/index.php";
                      params = [
                        {
                          name = "search";
                          value = "{searchTerms}";
                        }
                      ];
                    }
                  ];
                  definedAliases = [
                    "@w"
                    "@nw"
                  ];
                };
              };
            };
          };
        };
      };
    };

    services = {
      polkit-gnome.enable = true;
      hyprsunset.enable = true;
      wayle.enable = true;

      mpris-proxy.enable = true;
    };

    stylix.targets.gtksourceview.enable = false;

    xdg.configFile."hypr/.luarc.json".text = /* json */ ''
      {
        "workspace": {
          "library": [
            "${pkgs.hyprland}/share/hypr/stubs"
          ]
        }
      }
    '';

    internal.desktop.wallpaper.enable = lib.mkDefault true;
  };
}
