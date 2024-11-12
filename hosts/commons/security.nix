{
  security = {
    polkit.enable = true;
    pam = {
      services = {
        login.enableGnomeKeyring = true;
      };
    };
    rtkit.enable = true;
  };
}
