{
  pkgs,
  ...
}:
{
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

}
