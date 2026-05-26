{self, ...}: {
  flake.modules.homeManager.ranger = {pkgs, ...}: {
    home.file.".config/ranger" = {
      source = ./configs;
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
  };

  flake.modules.nixos.ranger = {
    home-manager.sharedModules = [
      self.modules.homeManager.ranger
    ];
  };
}
