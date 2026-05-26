{self, ...}: {
  flake.modules.homeManager.kitty = {pkgs, ...}: {
    home.packages = with pkgs; [
      jetbrains-mono
    ];

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
  };

  flake.modules.nixos.kitty = {
    home-manager.sharedModules = [
      self.modules.homeManager.kitty
    ];
  };
}
