{pkgs, ...}: {
  home.packages = with pkgs; [
    firefox
    google-chrome
    spotify
    discord
    whatsapp-for-linux
    telegram-desktop
    zathura
  ];
}
