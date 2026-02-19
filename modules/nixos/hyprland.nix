{
  lib,
  pkgs,
  config,
  ...
}: let
  cfg = config.services.myhypr;
in {
  options.services.myhypr = {
    enable = lib.mkEnableOption "myhypr service";
    loginUser = lib.mkOption {
      type = lib.types.str;
    };
  };

  config = lib.mkIf cfg.enable {
    programs.hyprland = {
      enable = true;
      xwayland.enable = true;
      package = pkgs.unstable.hyprland;
    };

    services.greetd = {
      enable = true;
      settings = rec {
        initial_session = {
          command = "${pkgs.hyprland}/bin/Hyprland";
          user = cfg.loginUser;
        };

        default_session = initial_session;
      };
    };

    xdg.portal.wlr.enable = true;
  };
}
