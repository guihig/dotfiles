{pkgs, ...}: {
  home.packages = with pkgs; [
    steam
    wineWowPackages.stable
    winetricks
    ferium
    (lutris.override {
      extraPkgs = pkgs: [
        # List package dependencies here
      ];
    })
  ];
}
