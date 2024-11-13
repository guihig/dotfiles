{pkgs, ...}: {
  services.xserver = {
    windowManager.awesome = {
      enable = true;
      package = pkgs.awesome-git;
      luaModules = with pkgs.luaPackages; [luarocks lua-cjson inspect];
    };
  };

  services.displayManager = {
    defaultSession = "none+awesome";
  };
}
