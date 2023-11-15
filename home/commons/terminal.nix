{pkgs, ...}: {
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
