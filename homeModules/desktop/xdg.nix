{
  config,
  lib,
  ...
}:
{
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
        "inode/directory" = "yazi.desktop";

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
}
