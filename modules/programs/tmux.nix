{self, ...}: {
  flake.modules.homeManager.tmux = {pkgs, ...}: {
    imports = [
      self.modules.homeManager.tmuxinator
    ];

    programs.tmux = {
      enable = true;
      terminal = "tmux-256color";
      baseIndex = 1;
      historyLimit = 10000;
      escapeTime = 10;
      mouse = true;
      shell = "${pkgs.fish}/bin/fish";
      package = pkgs.unstable.tmux;
      # tmuxinator.enable = true;
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

        bind-key -n 'C-M-\' copy-mode

        # Vi mappings for copy-mode: https://unix.stackexchange.com/a/585672/410321
        bind-key -T copy-mode-vi v send-keys -X begin-selection
        bind-key -T copy-mode-vi y send-keys -X copy-selection
        bind-key -T copy-mode-vi r send-keys -X rectangle-toggle
        # Also copy to system clipboard
        bind-key -T copy-mode-vi y send-keys -X copy-pipe-and-cancel 'xclip -sel clip -i'
      '';
    };
  };

  flake.modules.nixos.tmux = {
    home-manager.sharedModules = [
      self.modules.homeManager.tmux
    ];
  };
}
