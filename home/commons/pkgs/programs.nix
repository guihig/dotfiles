{pkgs, ...}: {
  home.packages = with pkgs; [
    firefox
    google-chrome
    chromium
    spotify
    discord
    zathura
    libreoffice
    gimp
    vesktop
    ventoy-full
    bitwarden
  ];
}
