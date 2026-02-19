{
  home.file.".config/dunst" = {
    source = ../../config/dunst;
    recursive = true;
  };

  services.dunst = {
    enable = true;
  };
}
