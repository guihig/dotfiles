{pkgs, ...}: {
  home.pointerCursor = {
    gtk.enable = true;
    x11.enable = true;
    package = pkgs.qogir-theme;
    name = "Qogir";
  };

  gtk = {
    enable = true;
    gtk3 = {
      extraConfig = {gtk-decoration-layout = "menu:";};
    };
    theme = {
      name = "Materia-dark";
      package = pkgs.materia-theme;
    };
    iconTheme = {
      name = "Papirus-Dark";
      package = pkgs.papirus-icon-theme;
    };
  };

  qt = {
    enable = true;
    platformTheme.name = "gtk";
  };
}
