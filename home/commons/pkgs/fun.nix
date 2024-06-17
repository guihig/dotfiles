{pkgs, ...}: {
  home.packages = with pkgs; [
    steam
    wineWowPackages.stable
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
