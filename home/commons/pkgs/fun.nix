{pkgs, ...}: {
  home.packages = with pkgs; [
    steam
    wineWowPackages.stable
    winetricks
    (lutris.override {
      extraPkgs = pkgs: [
        # List package dependencies here
      ];
    })
  ];
}
