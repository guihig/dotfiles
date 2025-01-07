{pkgs, ...}: {
  home.packages = with pkgs; [
    alacritty-theme
  ];

  # ---- Kitty Configuration ---- #
  programs.kitty = {
    enable = true;
    shellIntegration.enableFishIntegration = true;
    font = {
      name = "JetBrainsMono Nerd Font";
      package = pkgs.jetbrains-mono;
      size = 10;
    };
    themeFile = "Earthsong";
    settings = {
      background_opacity = "0.75";
      enable_audio_bell = false;
      update_check_interval = 0;
      confirm_os_window_close = 0;
    };
  };

  # ---- Alacritty Configuration ---- #
  programs.alacritty = {
    enable = true;
    settings = {
      general = [
        pkgs.alacritty-theme.gruvbox_material
      ];
      env = {
        TERM = "xterm-256color";
      };
      shell.program = "${pkgs.fish}/bin/fish";
      window = {
        opacity = 0.75;
        decorations = "None";
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
        normal = {
          family = "JetBrainsMono Nerd Font";
          style = "Regular";
        };
      };
    };
  };

  # ---- Starship Configuration ---- #
  programs.starship = {
    enable = true;
    enableFishIntegration = true;
  };
  home.file.".config/starship.toml" = {
    source = ../../config/starship/starship.toml;
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
    source = ../../config/tmuxinator;
    recursive = true;
  };

  # ---- Ranger Configuration ---- #
  home.file.".config/ranger" = {
    source = ../../config/ranger;
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
}
