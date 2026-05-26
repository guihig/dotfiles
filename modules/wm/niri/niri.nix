{
  self,
  inputs,
  ...
}: {
  flake.modules.homeManager.niri = {pkgs, ...}: {
    home.packages = with pkgs; [
      xwayland-satellite
      unstable.quickshell
      bibata-cursors
      orchis-theme
      tela-icon-theme
    ];

    xdg.configFile."niri/config.kdl".source = ./configs/config.kdl;

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

    imports = [
      # Import the dms-shell only available in unstable right now
      "${inputs.nixpkgs-unstable}/nixos/modules/programs/wayland/dms-shell.nix"
      "${inputs.nixpkgs-unstable}/nixos/modules/programs/dsearch.nix"
    ];

    programs.niri.enable = true;
    programs.niri.package = pkgs.unstable.niri;

    programs.dsearch = {
      enable = true;
      package = pkgs.unstable.dsearch;
    };

    programs.dms-shell = {
      enable = true;
      package = pkgs.unstable.dms-shell;

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
