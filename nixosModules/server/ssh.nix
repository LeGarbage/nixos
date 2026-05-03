{
  config,
  lib,
  ...
}:
let
  cfg = config.internal.server.ssh;
in
{
  imports = [ ];
  options = {
    internal.server.ssh.enable = lib.mkEnableOption "ssh server";
  };
  config = lib.mkIf cfg.enable {
    services = {
      openssh = {
        enable = true;

        settings = {
          PasswordAuthentication = false;
          ChallengeResponseAuthentication = false;
          KbdInteractiveAuthentication = false;
          PermitRootLogin = "no";
        };
      };

      # fail2ban.enable = true;
    };
  };
}
