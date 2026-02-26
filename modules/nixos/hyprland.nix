{
  lib,
  pkgs,
  config,
  inputs,
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
      package = inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.hyprland;
      portalPackage = inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.xdg-desktop-portal-hyprland;
    };

    services.greetd = {
      enable = true;
      settings = rec {
        initial_session = {
          command = "${inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.hyprland}/bin/Hyprland";
          user = cfg.loginUser;
        };

        default_session = initial_session;
      };
    };

    xdg.portal.wlr.enable = true;
  };
}
