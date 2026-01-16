{pkgs, ...}: {
  home.packages = with pkgs; [
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
    bitwarden-desktop
    obs-studio
  ];
}
