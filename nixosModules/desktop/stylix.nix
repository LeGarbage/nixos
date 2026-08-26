{
  pkgs,
  ...
}:
{
  stylix = {
    enable = true;
    base16Scheme = "${pkgs.base16-schemes}/share/themes/onedark.yaml";
    polarity = "dark";

    cursor = {
      package = pkgs.simp1e-cursors;
      name = "Simp1e-Dark";
      size = 24;
    };

    fonts = {
      monospace = {
        package = pkgs.nerd-fonts.commit-mono;
        name = "CommitMonoNerdFont";
      };
      serif = {
        package = pkgs.source-serif;
        name = "Source Serif 4";
      };
      sansSerif = {
        package = pkgs.source-sans;
        name = "Source Sans 3";
      };
    };
    targets = {
      # NOTE: Remove once https://github.com/nix-community/stylix/issues/2318 is fixed
      gtksourceview.enable = false;
    };
  };
}
