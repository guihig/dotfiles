{self, ...}: {
  flake.modules.homeMamanager.alacritty = {pkgs, ...}: {
    home.packages = with pkgs; [
      alacritty-theme
      jetbrains-mono
    ];

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
  };

  flake.modules.nixos.alacritty = {
    home-manager.sharedModules = [
      self.modules.homeManager.alacritty
    ];
  };
}
