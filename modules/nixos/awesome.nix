{
  lib,
  pkgs,
  config,
  ...
}: let
  cfg = config.services.myawesome;
in {
  options.services.myawesome = {
    enable = lib.mkEnableOption "myawesome service";
    loginUser = lib.mkOption {
      type = lib.types.str;
    };
    setupCommands = lib.mkOption {
      type = lib.types.str;
    };
  };

  config = lib.mkIf cfg.enable {
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

    services.xserver.displayManager.setupCommands = cfg.setupCommands;
  };
}
