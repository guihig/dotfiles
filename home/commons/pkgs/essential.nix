{pkgs, ...}: {
  home.packages = with pkgs; [
    firefox
    google-chrome
    spotify
    discord
    telegram-desktop
    zathura
    libreoffice
    gimp
    vesktop
    bitwarden
  ];
}
