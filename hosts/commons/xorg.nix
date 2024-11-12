{pkgs, ...}: {
  environment.extraInit = ''
    xset s off -dpms
  '';

  services.xserver = {
    enable = true;
    displayManager = {
      lightdm = {
        enable = true;
        greeters.gtk.enable = true;
      };
      defaultSession = "none+awesome";
    };
    windowManager.awesome = {
      enable = true;
      package = pkgs.awesome-git;
      luaModules = with pkgs.luaPackages; [luarocks lua-cjson inspect];
    };
    xkb = {
      layout = "us";
      variant = "intl";
    };
    exportConfiguration = true;
  };
}
