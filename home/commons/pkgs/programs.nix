{pkgs, ...}: {
  home.packages = with pkgs; [
    firefox
    google-chrome
    chromium
    spotify
    discord
    discord-canary
    zathura
    libreoffice
    gimp
    vesktop
    ventoy-full
    bitwarden
  ];
}
