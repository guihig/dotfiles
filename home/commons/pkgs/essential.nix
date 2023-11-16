{pkgs, ...}: {
  home.packages = with pkgs; [
    firefox
    google-chrome
    spotify
    discord
    mailspring
    zathura
  ];
}
