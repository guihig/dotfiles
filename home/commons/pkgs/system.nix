{pkgs, ...}: {
  imports = [
    ./audio.nix
    ./fonts.nix
    ./network.nix
  ];

  home.packages = with pkgs; [
    htop
    btop
    ncdu
    unzip
    jq
    ranger
    ripgrep
    fd
    killall
    pulseaudio
    fzf
    eza
    gnome-disk-utility
    sl
    neofetch
    cmatrix
    libnotify
    seahorse
    gparted
    flameshot
    gzip
    ntfs3g
    ueberzug
    nix-your-shell
    xss-lock
    slock
    bluez
    bluez-tools
    kazam
    bootiso
  ];
}
