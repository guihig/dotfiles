{self, ...}: {
  flake.modules.homeManager.git = {...}: {
    programs.git = {
      enable = true;
      lfs.enable = true;
      settings = {
        user = {
          name = "guihig";
          email = "guiih.ig@gmail.com";
        };
        alias = {
          s = "status";
          a = "add";
          p = "push";
          c = "commit";
        };
        init = {
          defaultBranch = "main";
        };
      };
    };
  };

  flake.modules.nixos.git = {
    home-manager.sharedModules = [
      self.modules.homeManager.git
    ];
  };
}
