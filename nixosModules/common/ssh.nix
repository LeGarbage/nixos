{
  config,
  ...
}:
let
  cfg = config.internal.common.ssh;
in
{
  imports = [ ];
  options = {
  };
  config = {
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
