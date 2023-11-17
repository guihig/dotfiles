{pkgs, ...}: {
  home.packages = with pkgs; [
    firefox
    google-chrome
    spotify
    discord
    mailspring
    whatsapp-for-linux
    telegram-desktop
    zathura
  ];
}
