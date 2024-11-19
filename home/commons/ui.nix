{pkgs, ...}: {
  home.packages = with pkgs; [
    papirus-icon-theme
    qogir-icon-theme
    qogir-theme
    bibata-cursors
    graphite-gtk-theme
    tela-icon-theme
    orchis-theme
    lxappearance
  ];

  home.pointerCursor = {
    gtk.enable = true;
    x11.enable = true;
    package = pkgs.bibata-cursors;
    name = "Bibata-Modern-Amber";
    size = 24;
  };

  gtk = {
    enable = true;
    gtk3 = {
      extraConfig = {gtk-decoration-layout = "menu:";};
    };
    theme = {
      name = "Orchis-Orange-Dark";
      package = pkgs.orchis-theme;
    };
    iconTheme = {
      name = "Tela-orange-dark";
      package = pkgs.tela-icon-theme;
    };
  };

  qt = {
    enable = true;
    platformTheme.name = "gtk";
  };
}
