{self, ...}: {
  flake.modules.homeManager.dunst = {
    services.dunst = {
      enable = true;
    };
    home.file.".config/dunst" = {
      source = ./configs;
      recursive = true;
    };
  };

  flake.modules.nixos.dunst = {
    home-manager.sharedModules = [
      self.modules.homeManager.dunst
    ];
  };
}
