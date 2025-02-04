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
      cursor_trail = 3;
      cursor_trail_decay = "0.1 0.4";
    };
  };

  # ---- Alacritty Configuration ---- #
  programs.alacritty = {
    enable = true;
    settings = {
      general = {
        import = ["${pkgs.alacritty-theme}/gruvbox_material_medium_dark.toml"];
      };
      env = {
        TERM = "xterm-256color";
      };
      terminal = {
        shell.program = "${pkgs.fish}/bin/fish";
      };
      window = {
        opacity = 0.75;
        decorations = "None";
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

      # Vi mappings for copy-mode: https://unix.stackexchange.com/a/585672/410321
      bind-key -T copy-mode-vi v send-keys -X begin-selection
      bind-key -T copy-mode-vi y send-keys -X copy-selection
      bind-key -T copy-mode-vi r send-keys -X rectangle-toggle
      # Also copy to system clipboard
      bind-key -T copy-mode-vi y send-keys -X copy-pipe-and-cancel 'xclip -sel clip -i'
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
