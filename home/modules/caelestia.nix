{inputs, ...}: {
  imports = [
    inputs.caelestia-shell.homeManagerModules.default
  ];
  programs.caelestia = {
    enable = true;
    systemd = {
      enable = false;
      target = "graphical-session.target";
      environment = [];
    };
    settings = {
      appearance.transparency.enabled = true;
      bar.status.showBattery = false;
      services.useFahrenheit = false;
      notifs.actionOnClick = true;
      paths = {
        wallpaperDir = "~/.config/wallpapers";
      };
    };
    cli = {
      enable = true;
      settings.theme.enableGtk = false;
    };
  };
}
