{self, ...}: {
  flake.modules.homeManager.gamers = {pkgs, ...}: {
    home.packages = with pkgs; [
      wineWow64Packages.stable
      protonplus
      winetricks
      unstable.wowup-cf
      (lutris.override {
        extraPkgs = pkgs: [
          # List package dependencies here
        ];
      })
    ];
  };

  flake.modules.nixos.gamers = {pkgs, ...}: {
    home-manager.sharedModules = [
      self.modules.homeManager.gamers
    ];

    programs.gamescope = {
      enable = true;
      # https://github.com/ValveSoftware/gamescope/issues/1924#issuecomment-3725667842
      package = pkgs.gamescope.overrideAttrs (_: {
        NIX_CFLAGS_COMPILE = ["-fno-fast-math"];
      });
    };

    # Steam Usage:
    #  env -u LD_PRELOAD gamescope -W 1920 -H 1080 -w 1920 -h 1080 -f --force-grab-cursor -- env LD_PRELOAD="$LD_PRELOAD" %command%
    programs.steam = {
      enable = true;
      extraCompatPackages = [
        pkgs.proton-ge-bin
      ];
    };
  };
}
