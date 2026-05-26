{
  flake.modules.homeManager.rofi = {pkgs, ...}: {
    programs.rofi.enable = true;

    home.file.".config/rofi" = {
      source = ./configs;
      recursive = true;
    };
  };
}
