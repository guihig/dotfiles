{pkgs, ...}: {
  environment.extraInit = ''
    xset s off -dpms
  '';

  services.xserver = {
    enable = true;
    desktopManager.xterm.enable = false;
    windowManager.awesome = {
      enable = true;
      package = pkgs.awesome-git;
      luaModules = with pkgs.luaPackages; [luarocks lua-cjson inspect];
    };
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

  services.displayManager = {
    defaultSession = "none+awesome";
  };
}
