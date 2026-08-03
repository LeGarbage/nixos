{
  ...
}:
{
  services = {
    greetd = {
      enable = true;
    };
    displayManager.regreet = {
      enable = true;
      cageArgs = [
        "-s"
        "-d"
        "-m"
        "last"
      ];
      settings = {
        skip_selection = true;
      };
    };
  };
}
