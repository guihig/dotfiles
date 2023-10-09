# This is your home-manager configuration file
# Use this to configure your home environment (it replaces ~/.config/nixpkgs/home.nix)
{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  ...
}: {
  # You can import other home-manager modules here
  imports = [
    # If you want to use modules your own flake exports (from modules/home-manager):
    # outputs.homeManagerModules.example

    # Or modules exported from other flakes (such as nix-colors):
    # inputs.nix-colors.homeManagerModules.default

    # You can also split up your configuration and import pieces of it here:
    # ./nvim.nix

    # Sops
    inputs.sops-nix.homeManagerModule
  ];

  nixpkgs = {
    # You can add overlays here
    overlays = [
      # Add overlays your own flake exports (from overlays and pkgs dir):
      outputs.overlays.additions
      outputs.overlays.modifications
      outputs.overlays.unstable-packages
    ];
    # Configure your nixpkgs instance
    config = {
      # Disable if you don't want unfree packages
      allowUnfree = true;
      # Workaround for https://github.com/nix-community/home-manager/issues/2942
      allowUnfreePredicate = _: true;
    };
  };

  home = {
    username = "ferreira";
    homeDirectory = "/home/ferreira";
    pointerCursor = {
      gtk.enable = true;
      x11.enable = true;
      package = pkgs.qogir-theme;
      name = "Qogir";
    };
  };

  home.packages = with pkgs; [
    # Very Fun and games
    firefox
    google-chrome
    spotify
    discord
    mailspring
    steam

    # System utilities
    htop
    ncdu
    unzip
    jq
    ranger
    ripgrep
    fd
    killall
    pulseaudio
    fzf
    exa
    xcowsay
    xclip
    gnome.gnome-disk-utility
    sl
    neofetch
    cmatrix
    libnotify
    gnome.seahorse
    gparted
    flameshot
    gzip
    ntfs3g
    papirus-icon-theme
    qogir-icon-theme
    qogir-theme
    arandr
    ueberzug
    zathura
    lxappearance
    nix-your-shell
    xss-lock
    slock

    # soundsss
    pavucontrol
    alsa-utils
    vlc

    # networking
    openfortivpn
    networkmanagerapplet

    # very cool code kid0
    vim
    nano
    vscode
    cargo
    rustc
    go
    gcc
    gnumake
    lua
    elixir
    nodejs
    tree-sitter
    luaPackages.luarocks
    (let
      python3-with-packages = pkgs.python3.withPackages (p:
        with p; [
          pynvim
          setuptools
          pip
        ]);
    in
      python3-with-packages)
    jetbrains.pycharm-professional
    docker-compose
    postgresql_13

    # Lsps
    lua-language-server
    # nodePackages.vscode-langservers-extracted
    # nodePackages.dockerfile-language-server-nodejs
    # nodePackages.typescript-language-server
    # nodePackages.yaml-language-server
    # nodePackages.pyright
    # nodePackages.volar
    texlab
    # sqls
    # efm-langserver
    # rnix-lsp
    # rust-analyzer
    # elixir-ls

    # Code formatters
    stylua
    yamlfmt
    black
    nodePackages.sql-formatter
    stylish-haskell
    alejandra
    texlive.combined.scheme-full

    # Fonts
    nerdfonts
    iosevka
    iosevka-bin
    siji
    termsyn
    jetbrains-mono
    material-icons
    material-design-icons
    terminus_font
    terminus-nerdfont
    font-awesome
    noto-fonts
    liberation_ttf
  ];

  # Enable home-manager and git
  programs.home-manager.enable = true;

  # ---- Git Configuration ---- #
  programs.git = {
    enable = true;
    lfs.enable = true;
    userName = "guihig";
    userEmail = "guiih.ig@gmail.com";
    aliases = {
      s = "status";
      a = "add";
      p = "push";
      c = "commit";
    };
  };

  # ---- fish Configuration ---- #
  programs.fish = {
    enable = true;
    shellAliases = {
      pong = "ping 8.8.8.8";
      tmx = "tmuxinator";
      vim = "nvim";
      myip = "curl -fSsL 'https://api.ipify.org?format=json' | jq \".ip\"";
      ssh = "TERM=xterm-256color ssh";
      cowalert = "xcowsay --monitor 1 comando: \" $(history | tail -n1 | grep -oP '\''(?<=  )[^;]++'\'' | head -n1) \" acabou ";
      toclip = "xclip -selection clipboard";
      granter = "cd /home/ferreira/dev/granter/";
      ls = "exa";
      ll = "exa -la";
      ip = "ip -c";
      ciasc-vpn = "sudo openfortivpn sslvpn01.ciasc.gov.br:443 --username=granter_gferreira@vpn.ciasc.gov.br";
    };
    interactiveShellInit = ''
      set fish_greeting
      set EDITOR "vim"

      nix-your-shell fish | source
    '';
  };

  # ---- Alacritty Configuration ---- #
  programs.alacritty = {
    enable = true;
    settings = {
      env = {
        TERM = "xterm-256color";
      };
      shell.program = "${pkgs.fish}/bin/fish";
      window = {
        opacity = 0.85;
        padding = {
          x = 0;
          y = 0;
        };
        class = {
          instance = "Alacritty";
          general = "Alacritty";
        };
      };
      font = {
        size = 10;
        normal = {
          font = "JetBrainsMono Nerd Font";
          style = "Regular";
        };
      };
      colors = {
        primary = {
          background = "0x000000";
          foreground = "0xB3B1AD";
        };
      };
      # colors = {
      #   primary = {
      #     background = "0x000000";
      #     foreground = "0xB3B1AD";
      #   };
      #   normal = {
      #     black = "0x01060E";
      #     red = "0xEA6C73";
      #     green = "0x91B362";
      #     yellow = "0xF9AF4F";
      #     blue = "0x53BDFA";
      #     magenta = "0xFAE994";
      #     purple = "0x9C39FF";
      #     cyan = "0x90E1C6";
      #     white = "0xC7C7C7";
      #   };
      #   bright = {
      #     black = "0x686868";
      #     red = "0xF07178";
      #     green = "0xC2D94C";
      #     yellow = "0xFFB454";
      #     blue = "0x59C2FF";
      #     magenta = "0xFFEE99";
      #     purple = "0x9C39FF";
      #     cyan = "0x95E6CB";
      #     white = "0xFFFFFF";
      #   };
      # };
    };
  };

  # ---- Starship Configuration ---- #
  programs.starship = {
    enable = true;
    enableFishIntegration = true;
  };
  home.file.".config/starship.toml" = {
    source = ../config/starship/starship.toml;
  };

  # ---- tmux Configuration ---- #
  programs.tmux = {
    enable = true;
    terminal = "tmux-256color";
    baseIndex = 1;
    historyLimit = 10000;
    escapeTime = 10;
    mouse = true;
    shell = "${pkgs.fish}/bin/fish";
    tmuxinator.enable = true;
    plugins = with pkgs; [
      {
        plugin = tmuxPlugins.tilish;
        extraConfig = ''
          set -g @tilish-easymode 'on'
        '';
      }
      {
        plugin = tmuxPlugins.power-theme;
        extraConfig = ''
          set -g @tmux_power_theme 'snow'
        '';
      }
    ];
    extraConfig = ''
      unbind C-b
      set-option -g prefix C-a
      bind-key C-a send-prefix

      set-option -ga terminal-overrides ",*256col*:Tc:RGB"
      set -g base-index 1
      setw -g pane-base-index 1
    '';
  };

  # ---- tmuxinator Configuration ---- #
  home.file.".config/tmuxinator" = {
    source = ../config/tmuxinator;
    recursive = true;
  };

  # ---- Ranger Configuration ---- #
  home.file.".config/ranger" = {
    source = ../config/ranger;
    recursive = true;
  };
  home.file.".config/ranger/plugins/ranger_devicons" = {
    source = pkgs.fetchFromGitHub {
      owner = "alexanderjeurissen";
      repo = "ranger_devicons";
      rev = "de64ab26fb581c00a803381d522c6b3e48b79415";
      hash = "sha256-6JEhyU08QEkGdRW2L00ynRaoaaR5PaiVUccEUbtTQuU=";
    };
  };

  # ---- Rofi Configuration ---- #
  programs.rofi = {
    enable = true;
  };
  home.file.".config/rofi" = {
    source = ../config/rofi;
    recursive = true;
  };

  # ---- nvim Configuration ---- #
  programs.neovim.enable = true;
  home.file.".config/nvim" = {
    source = ../config/nvim;
    recursive = true;
  };

  # ---- Awesome Configuration ---- #
  xsession.windowManager.awesome = {
    enable = true;
  };
  home.file.".config/wallpapers" = {
    source = ../wallpapers;
    recursive = true;
  };
  home.file.".config/awesome" = {
    source = ../config/awesome;
    recursive = true;
  };
  home.file.".config/awesome/modules/bling" = {
    source = inputs.bling.outPath;
    recursive = true;
  };
  home.file.".config/awesome/modules/rubato" = {
    source = inputs.rubato.outPath;
    recursive = true;
  };
  home.file.".config/awesome/lain" = {
    source = inputs.lain.outPath;
    recursive = true;
  };

  # ---- Xresources colors :) ---- #
  xresources.extraConfig = builtins.readFile (
    pkgs.fetchFromGitHub {
      owner = "rebelot";
      repo = "kanagawa.nvim";
      rev = "58cdce2cb666e6e946edec0145f177f89ca4a9ad";
      sha256 = "sha256-TmuwIhjptegMcdwYToCTS9dyyndKzp5fJoahXF3F1K0=";
    }
    + "/extras/.Xresources"
  );

  # ---- SSH Configs ---- #
  home.file.".ssh/config" = {
    source = ../config/ssh/config;
  };

  # ---- Picom Config ---- #
  services.picom = {
    enable = true;
    fade = true;
    fadeSteps = [0.1 0.1];
    shadow = true;
    shadowOffsets = [(-7) (-7)];
    shadowExclude = [
      "name = 'Notification'"
      "class_g = 'Conky'"
      "class_g ?= 'Notify-osd'"
      "class_g = 'Cairo-clock'"
      "class_g = ''"
      "_GTK_FRAME_EXTENTS@:c"
    ];
    inactiveOpacity = 0.9;
    wintypes = {
      tooltip = {
        fade = true;
        shadow = true;
        opacity = 0.75;
        focus = true;
        full-shadow = false;
      };
      dock = {
        shadow = false;
        clip-shadow-above = true;
      };
      dnd = {shadow = false;};
      popup_menu = {
        shadow = false;
        fade = false;
      };
    };
    backend = "glx";
    opacityRules = [
      "100:name = 'Picture-in-Picture'"
      "100:fullscreen"
    ];
    settings = {
      transparent-clipping = true;
      inactive-opacity-override = false;
      frame-opacity = 1.0;
      rounded-corners-exclude = [
        "window_type = 'dock'"
        "window_type = 'desktop'"
      ];
      blur-kern = "3x3box";
      blur-background-exclude = [
        "window_type = 'dock'"
        "window_type = 'desktop'"
        "_GTK_FRAME_EXTENTS@:c"
      ];
      glx-no-stencil = true;
      shadow-radius = 7;
    };
  };

  gtk = {
    enable = true;
    gtk3 = {
      extraConfig = {gtk-decoration-layout = "menu:";};
    };
    theme = {
      name = "Materia-dark";
      package = pkgs.materia-theme;
    };
    iconTheme = {
      name = "Papirus-Dark";
      package = pkgs.papirus-icon-theme;
    };
  };

  qt = {
    enable = true;
    platformTheme = "gtk";
  };

  # Sops
  sops = {
    age.sshKeyPaths = ["/etc/ssh/id_ed25519"];
    defaultSopsFile = ../secrets/common/secrets.yaml;

    secrets."ssh_keys/id_rsa/priv" = {
      path = "${config.home.homeDirectory}/.ssh/id_rsa";
      mode = "0600";
    };
    secrets."ssh_keys/id_rsa/pub" = {
      path = "${config.home.homeDirectory}/.ssh/id_rsa.pub";
      mode = "0644";
    };

    secrets."ssh_keys/ciasc_rsa/priv" = {
      path = "${config.home.homeDirectory}/.ssh/ciasc_rsa";
      mode = "0600";
    };
    secrets."ssh_keys/ciasc_rsa/pub" = {
      path = "${config.home.homeDirectory}/.ssh/ciasc_rsa.pub";
      mode = "0644";
    };

    secrets."ssh_keys/owls_cloud/pem" = {
      path = "${config.home.homeDirectory}/.ssh/owls_cloud.pem";
      mode = "0600";
    };

    secrets."ssh_keys/google_compute_engine/priv" = {
      path = "${config.home.homeDirectory}/.ssh/google_compute_engine";
      mode = "0600";
    };
    secrets."ssh_keys/google_compute_engine/pub" = {
      path = "${config.home.homeDirectory}/.ssh/google_compute_engine.pub";
      mode = "0644";
    };
  };

  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  home.stateVersion = "23.05";
}
