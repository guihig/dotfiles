{pkgs, ...}: {
  home.packages = with pkgs; [
    # System utilities
    htop
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
    xcowsay
    xclip
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
    arandr
    ueberzug
    lxappearance
    nix-your-shell
    xss-lock
    slock
    bluez
    bluez-tools
    kazam
    xwaylandvideobridge
    xdg-desktop-portal-hyprland
    bootiso

    # Wayland
    waybar
    wl-gammactl
    wl-clipboard
    wf-recorder
    wlprop
    hyprpicker
    wayshot
    swappy
    grim
    slurp
    imagemagick
    swww

    # Audio
    pavucontrol
    alsa-utils
    vlc

    # Codecs
    ffmpeg

    # Networking
    openfortivpn
    networkmanagerapplet

    # Fonts
    nerdfonts
    iosevka
    iosevka-bin
    siji
    termsyn
    jetbrains-mono
    material-icons
    material-design-icons
    terminus_font
    terminus-nerdfont
    font-awesome
    noto-fonts
    liberation_ttf
  ];
}
