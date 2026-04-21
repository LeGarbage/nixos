{
  pkgs,
  config,
  ...
}:
let
  cfg = config.internal.common;
in
{
  imports = [
    ./nix.nix
  ];
  options = {
  };
  config = {
    programs = {
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
          merge.tool = "codediff";
          mergetool.codediff.cmd = ''nvim "$MERGED" -c "CodeDiff merge \"$MERGED\""'';
          diff.tool = "codediff";
          difftool.codediff.cmd = ''nvim "$LOCAL" "$REMOTE" +"CodeDiff file $LOCAL $REMOTE"'';
        };
      };

      direnv = {
        enable = true;
        nix-direnv.enable = true;
      };

      btop = {
        enable = true;
      };

      # Let Home Manager install and manage itself.
      home-manager.enable = true;
    };

    home.packages = with pkgs; [
      yazi
    ];
  };
}
