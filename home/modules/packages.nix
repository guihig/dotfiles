{pkgs, ...}: {
  home.packages = with pkgs; [
    # General
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

    # Audio
    pavucontrol
    alsa-utils
    vlc
    noisetorch

    # Codecs
    ffmpeg

    # Code packages
    cargo
    rustc
    go
    gcc
    gnumake
    lua51Packages.lua
    lua51Packages.luarocks
    kdePackages.qtdeclarative
    nodejs
    tree-sitter
    (let
      python3-with-packages = pkgs.python3.withPackages (p:
        with p; [
          pynvim
          setuptools
          pip
          psycopg2
          ds4drv
        ]);
    in
      python3-with-packages)
    docker-compose
    postgresql
    insomnia
    code-minimap
    zrok
    nodePackages.localtunnel

    # Code formatters
    stylua
    yamlfmt
    black
    nodePackages.sql-formatter
    nodePackages.prettier
    stylish-haskell
    alejandra
    texlive.combined.scheme-full

    # Editors
    vim
    nano
    vscode
    jetbrains.pycharm
    github-desktop

    # Network/Vpns
    openfortivpn
    networkmanagerapplet

    # FUN
    steam
    stremio
    wineWowPackages.stable
    protonplus
    winetricks
    unstable.wowup-cf
    protonup-qt
    gamescope
    (lutris.override {
      extraPkgs = pkgs: [
        # List package dependencies here
      ];
    })

    # Fonts
    nerd-fonts.jetbrains-mono
    nerd-fonts.terminess-ttf
    nerd-fonts.hack

    iosevka
    roboto
    iosevka-bin
    siji
    termsyn
    jetbrains-mono
    material-icons
    material-design-icons
    terminus_font
    font-awesome
    noto-fonts
    liberation_ttf

    # System packages
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
    p7zip
    ntfs3g
    ueberzug
    nix-your-shell
    xss-lock
    slock
    bluez
    bluez-tools
    kazam
    bootiso
    mangohud
    goverlay
    mangojuice
    lm_sensors
    hddtemp
  ];
}
