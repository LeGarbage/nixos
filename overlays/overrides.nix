final: prev: {
  rofi = prev.rofi.override { plugins = [ prev.rofi-calc ]; };
  kdePackages = prev.kdePackages // {
    # Merkuro needs this environment variable set to be properly themed
    merkuro = prev.symlinkJoin {
      name = "merkuro";
      paths = [ prev.kdePackages.merkuro ];
      nativeBuildInputs = [ prev.makeWrapper ];
      postBuild = ''
        wrapProgram $out/bin/merkuro-calendar --set QT_QPA_PLATFORMTHEME kde
      '';
    };
  };
}
