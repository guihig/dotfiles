{pkgs, ...}: {
  home.packages = with pkgs; [
    steam
    stremio
    wineWowPackages.stable
    protonplus
    winetricks
    unstable.wowup-cf
    protonup-qt
    gamescope
    (lutris.override {
      extraPkgs = pkgs: [
        # List package dependencies here
      ];
    })
  ];
}
