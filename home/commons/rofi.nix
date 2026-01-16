{pkgs, ...}: {
  # ---- Rofi Configuration ---- #
  programs.rofi = {
    enable = true;
  };
  home.file.".config/rofi" = {
    source = ../../config/rofi;
    recursive = true;
  };
}
