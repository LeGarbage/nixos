{
  lib,
  config,
  ...
}:
let
  cfg = config.internal.desktop.wallpaper;
in
{
  options = {
    internal.desktop.wallpaper.defaultPath = lib.mkOption {
      type = lib.types.path;
      description = "Path to image to be used as the dafault wallpaper";
    };
  };

  config = {
    services.wpaperd = {
      enable = true;
      settings = {
        any = {
          path = cfg.defaultPath;
        };
      };
    };
  };
}
