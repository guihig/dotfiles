{pkgs, ...}: {
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
    windowManager.awesome = {
      enable = true;
      package = pkgs.awesome-git;
      luaModules = with pkgs.luaPackages; [luarocks lua-cjson inspect];
    };
  };

  services.displayManager = {
    defaultSession = "none+awesome";
  };

  environment.extraInit = ''
    xset s off -dpms
  '';
}
