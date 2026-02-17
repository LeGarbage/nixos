{
  config,
  lib,
  ...
}:
let
  cfg = config.internal.common.nix;

  presets = {
    aggressive = {
      nix = {
        gc = {
          automatic = true;
          options = "--delete-older-than 7d";
          dates = "daily";
        };
        settings.auto-optimise-store = true;
      };
    };
    normal = {
      nix = {
        gc = {
          automatic = true;
          options = "--delete-older-than 14d";
          dates = "weekly";
        };
        optimise = {
          automatic = true;
          dates = "weekly";
        };
      };
    };
    minimal = {
      nix = {
        gc = {
          automatic = true;
          options = "--delete-older-than 28d";
          dates = "weekly";
        };
        optimise = {
          automatic = true;
          dates = "weekly";
        };
      };
    };
  };
in
{
  imports = [ ];
  options = {
    internal.common.nix.storeStrategy = lib.mkOption {
      description = ''
        How agressively to optimize/gc the nix store
      '';
      type = lib.types.enum [
        "aggressive"
        "normal"
        "minimal"
      ];
    };
  };
  config = lib.mkMerge [
    {
      nixpkgs.config.allowUnfree = true;
      nix = {
        settings = {
          # Use flakes
          experimental-features = [
            "nix-command"
            "flakes"
          ];
        };
      };
    }
    (lib.mkIf (cfg.storeStrategy == "aggressive") presets.aggressive)
    (lib.mkIf (cfg.storeStrategy == "normal") presets.normal)
    (lib.mkIf (cfg.storeStrategy == "minimal") presets.minimal)
  ];
}
