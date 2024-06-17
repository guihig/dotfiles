{pkgs, ...}: {
  # ---- Rofi Configuration ---- #
  programs.rofi = {
    enable = true;
    package = pkgs.rofi-wayland;
  };
  home.file.".config/rofi" = {
    source = ../../config/rofi;
    recursive = true;
  };
}
