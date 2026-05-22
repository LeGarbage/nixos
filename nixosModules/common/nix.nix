{
  config,
  lib,
  ...
}:
let
  cfg = config.internal.common.nix;

  presets = {
    aggressive = {
      nix.settings.auto-optimise-store = true;
      programs.nh.clean = {
        enable = true;
        extraArgs = "--keep 3 --keep-since 1w";
        dates = "daily";
      };
    };
    normal = {
      programs.nh.clean = {
        enable = true;
        extraArgs = "--keep 6 --keep-since 2w --optimise";
        dates = "weekly";
      };
    };
    minimal = {
      programs.nh.clean = {
        enable = true;
        extraArgs = "--keep 12 --keep-since 4w --optimise --no-gcroots";
        dates = "weekly";
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
      programs.nh.enable = true;
    }
    (lib.mkIf (cfg.storeStrategy == "aggressive") presets.aggressive)
    (lib.mkIf (cfg.storeStrategy == "normal") presets.normal)
    (lib.mkIf (cfg.storeStrategy == "minimal") presets.minimal)
  ];
}
