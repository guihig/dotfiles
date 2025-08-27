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
    audacity
    vesktop
    ventoy-full
    figma-linux
    bitwarden
    obs-studio
  ];
}
