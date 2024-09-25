{pkgs, ...}: {
  home.packages = with pkgs; [
    firefox
    google-chrome
    spotify
    discord
    zathura
    libreoffice
    gimp
    vesktop
    bitwarden
  ];
}
