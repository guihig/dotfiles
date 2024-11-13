{pkgs, ...}: {
  imports = [
    ./system.nix
    ./programs.nix
    ./code.nix
    ./formatters.nix
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
    gnome.gnome-disk-utility
    sl
    neofetch
    cmatrix
    libnotify
    gnome.seahorse
    gparted
    flameshot
    gzip
    ntfs3g
    papirus-icon-theme
    qogir-icon-theme
    qogir-theme
    ueberzug
    lxappearance
    nix-your-shell
    xss-lock
    slock
    bluez
    bluez-tools
    kazam
    bootiso
  ];
}
