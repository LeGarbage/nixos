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
      };
    };
    normal = {
      nix = {
        gc = {
          automatic = true;
          options = "--delete-older-than 14d";
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
      };
    };
  };
in
{
  imports = [ ];
  options = {
    internal.common.nix.storeStrategy = lib.mkOption {
      description = ''
        How agressively to gc the user's nix store
      '';
      type = lib.types.enum [
        "aggressive"
        "normal"
        "minimal"
      ];
    };
  };
  config = lib.mkMerge [
    (lib.mkIf (cfg.storeStrategy == "aggressive") presets.aggressive)
    (lib.mkIf (cfg.storeStrategy == "normal") presets.normal)
    (lib.mkIf (cfg.storeStrategy == "minimal") presets.minimal)
  ];
}
