{
  ...
}:
{
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
}
