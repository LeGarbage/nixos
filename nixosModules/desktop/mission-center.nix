{
  pkgs,
  ...
}:
{
  environment.systemPackages = with pkgs; [ mission-center ];
  # Needed to report CPU power draw
  services.udev.extraRules = ''
    SUBSYSTEM=="powercap", KERNEL=="intel-rapl*", \
      RUN+="${pkgs.coreutils}/bin/chmod -R a+r /sys/%p/"
  '';
}
