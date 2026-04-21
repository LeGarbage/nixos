final: prev: {
  wrappedNeovim =
    let
      buildInputs = with prev; [
        # Neovim
        wl-clipboard
        ripgrep
        tree-sitter
        git
        fd

        # Blink
        curl

        # Telescope-fzf-native
        gcc
        gnumake
        # Snacks dashboard
        fortune
        # Direnv.nvim
        direnv

        # nil and nixfmt should be installed globally
        nixfmt
        nil
      ];
    in
    prev.symlinkJoin {
      name = "nvim";
      paths = [ prev.neovim ];
      nativeBuildInputs = [ prev.makeWrapper ];
      buildInputs = buildInputs;
      postBuild = ''
        wrapProgram $out/bin/nvim --prefix PATH : \
            ${prev.lib.makeBinPath buildInputs}
      '';
    };
}
