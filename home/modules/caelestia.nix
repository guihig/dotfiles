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
      services = {
        weatherLocation = "-27.59730,-48.54961";
        useFahrenheit = false;
      };
      notifs.actionOnClick = true;
      paths = {
        wallpaperDir = "~/.config/wallpapers";
      };
      bar = {
        status.showBattery = false;
      };
    };
    cli = {
      enable = true;
      settings.theme.enableGtk = false;
    };
  };
}
