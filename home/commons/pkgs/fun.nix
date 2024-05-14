{pkgs, ...}: {
  home.packages = with pkgs; [
    steam
    wineWowPackages.stable
    winetricks
    unstable.wowup-cf
    (lutris.override {
      extraPkgs = pkgs: [
        # List package dependencies here
      ];
    })
  ];
}
