{self, ...}: {
  flake-file.inputs = {
    bling = {
      url = "github:BlingCorp/bling";
      flake = false;
    };
    rubato = {
      url = "github:andOrlando/rubato";
      flake = false;
    };
    lain = {
      url = "github:lcpz/lain";
      flake = false;
    };
    awesome-wm-widgets = {
      url = "github:streetturtle/awesome-wm-widgets";
      flake = false;
    };
    color = {
      url = "github:andOrlando/color";
      flake = false;
    };
  };

  flake.modules.homeManager.awesomewm = {pkgs, ...}: {
    imports = [
      self.modules.homeManager.fastcompmgr
      self.modules.homeManager.rofi
    ];

    home.packages = with pkgs; [
      # papirus-icon-theme
      qogir-icon-theme
      qogir-theme
      bibata-cursors
      graphite-gtk-theme
      tela-icon-theme
      orchis-theme
      lxappearance
      arandr
      xlayoutdisplay
    ];

    # ---- Awesome Configuration ---- #
    xsession.windowManager.awesome = {
      enable = true;
    };
    home.file.".config/wallpapers" = {
      source = ../../../wallpapers;
      recursive = true;
    };
    home.file.".config/awesome" = {
      source = ./configs;
      recursive = true;
    };
    home.file.".config/awesome/modules/bling" = {
      source = self.inputs.bling.outPath;
      recursive = true;
    };
    home.file.".config/awesome/modules/rubato" = {
      source = self.inputs.rubato.outPath;
      recursive = true;
    };
    home.file.".config/awesome/modules/color" = {
      source = self.inputs.color.outPath;
      recursive = true;
    };
    home.file.".config/awesome/lain" = {
      source = self.inputs.lain.outPath;
      recursive = true;
    };
    home.file.".config/awesome/awesome-wm-widgets" = {
      source = self.inputs.awesome-wm-widgets.outPath;
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

    xresources.extraConfig = builtins.readFile (
      pkgs.fetchFromGitHub {
        owner = "rebelot";
        repo = "kanagawa.nvim";
        rev = "58cdce2cb666e6e946edec0145f177f89ca4a9ad";
        sha256 = "sha256-TmuwIhjptegMcdwYToCTS9dyyndKzp5fJoahXF3F1K0=";
      }
      + "/extras/.Xresources"
    );
  };

  flake.modules.nixos.awesomewm = {pkgs, ...}: {
    home-manager.sharedModules = [
      self.modules.homeManager.awesomewm
    ];

    programs = {
      slock.enable = true;

      xss-lock = {
        enable = true;
        lockerCommand = "${pkgs.slock}/bin/slock";
      };
    };

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
        luaModules = with pkgs.luajitPackages; [luarocks cjson inspect lgi];
      };
    };

    services.displayManager = {
      defaultSession = "none+awesome";
    };

    environment.extraInit = ''
      xset s off -dpms
    '';

    services.xserver.displayManager.setupCommands = ''
      LEFT='DP-2'
      CENTER='DP-4'
      TOP='DP-0'

      ${pkgs.xorg.xrandr}/bin/xrandr \
        --output $LEFT --mode 3440x1440 --rate 144 --pos -3440x25 --rotate normal  \
        --output $CENTER --primary --mode 1920x1080 --rate 240 --pos 0x0 --rotate normal \
        --output $TOP --mode 1920x1080 --pos 270x-1080 --rotate normal
    '';

    xdg.portal = {
      enable = true;
      extraPortals = [pkgs.xdg-desktop-portal-gtk];
      config.common.default = "gtk";
    };

    services.gnome.gnome-keyring.enable = true;
    security.polkit.enable = true;
    security.pam.services.login.enableGnomeKeyring = true;
  };
}
