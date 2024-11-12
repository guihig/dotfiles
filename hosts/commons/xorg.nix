{pkgs, ...}: {
  environment.extraInit = ''
    xset s off -dpms
  '';

  services.xserver = {
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

  services = {
    greetd = {
      enable = true;
      settings = rec {
        initial_session = {
          command = "${pkgs.awesome-git}/bin/awesome";
          user = "ferreira";
        };
        default_session = initial_session;
      };
    };
  };
}
