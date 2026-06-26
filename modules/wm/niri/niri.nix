{self, ...}: {
  flake-file.inputs = {
    ns-flake = {
      url = "github:gvolpe/niri-scratchpad";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  flake.modules.homeManager.niri = {pkgs, ...}: let
    inherit (self.inputs.ns-flake.packages.${pkgs.stdenv.hostPlatform.system}) niri-scratchpad;
  in {
    home.packages = with pkgs; [
      niri-scratchpad
      wlprop
      xwayland-satellite
      unstable.quickshell
      bibata-cursors
      orchis-theme
      tela-icon-theme
    ];

    home.file.".config/niri" = {
      source = ./configs;
      recursive = true;
    };

    home.pointerCursor = {
      gtk.enable = true;
      x11.enable = true;
      package = pkgs.bibata-cursors;
      name = "Bibata-Modern-Amber";
      size = 24;
    };

    gtk = {
      enable = true;
      gtk3 = {
        extraConfig = {gtk-decoration-layout = "menu:";};
      };
      theme = {
        name = "Orchis-Orange-Dark";
        package = pkgs.orchis-theme;
      };
      iconTheme = {
        name = "Tela-orange-dark";
        package = pkgs.tela-icon-theme;
      };
    };

    qt = {
      enable = true;
      platformTheme.name = "gtk";
    };

    dconf.settings = {
      "org/gnome/desktop/interface" = {
        color-scheme = "prefer-dark";
      };
    };
  };

  flake.modules.nixos.niri = {
    pkgs,
    config,
    ...
  }: {
    home-manager.sharedModules = [
      self.modules.homeManager.niri
    ];

    programs.niri.enable = true;
    programs.niri.package = pkgs.unstable.niri;

    programs.dsearch = {
      enable = true;
    };

    programs.dms-shell = {
      enable = true;
      enableSystemMonitoring = false;
    };

    environment.systemPackages = with pkgs; [unstable.dgop];

    systemd.user.services.niri.enableDefaultPath = false;

    services.greetd = {
      enable = true;
      settings = rec {
        initial_session = {
          command = "${config.programs.niri.package}/bin/niri-session";
          user = "ferreira";
        };

        default_session = initial_session;
      };
    };

    services.gnome.gnome-keyring.enable = true;
    security.polkit.enable = true;
    security.pam.services.login.enableGnomeKeyring = true;
  };
}
