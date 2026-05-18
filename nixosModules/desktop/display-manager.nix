{
  config,
  pkgs,
  ...
}:
let
  cfg = config.internal.desktop.displayManager;
in
{
  imports = [ ];
  options = { };
  config = {
    services = {
      greetd = {
        enable = true;
      };
    };

    programs.regreet = {
      enable = true;
      cageArgs = [
        "-s"
        "-d"
        "-m"
        "last"
      ];
    };
  };
}
