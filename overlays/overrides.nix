final: prev: {
  pkgsStable = import final.inputs.nixpkgs-stable {
    system = final.system;
  };
  rofi = prev.rofi.override { plugins = [ prev.rofi-calc ]; };
  blesh = final.pkgsStable.blesh;
}
