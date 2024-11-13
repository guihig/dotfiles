{pkgs, ...}: {
  environment.extraInit = ''
    xset s off -dpms
  '';

  services.xserver = {
    enable = true;
    desktopManager.xterm.enable = false;
    xkb = {
      layout = "us";
      variant = "intl";
    };
    displayManager = {
      lightdm = {
        enable = true;
        greeters.gtk.enable = true;
      };
    };
    exportConfiguration = true;
  };
}
