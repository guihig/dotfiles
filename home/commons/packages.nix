# This is your home-manager configuration file
# Use this to configure your home environment (it replaces ~/.config/nixpkgs/home.nix)
{pkgs, ...}: {
  home.packages = with pkgs; [
    # Very Fun and games
    firefox
    google-chrome
    spotify
    discord
    mailspring
    steam
    wineWowPackages.stable
    winetricks
    (lutris.override {
      extraPkgs = pkgs: [
        # List package dependencies here
      ];
    })

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
    exa
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
    zathura
    lxappearance
    nix-your-shell
    xss-lock
    slock
    bluez
    bluez-tools
    kazam

    # soundsss
    pavucontrol
    alsa-utils
    vlc

    # networking
    openfortivpn
    networkmanagerapplet

    # very cool code kid0
    vim
    nano
    vscode
    cargo
    rustc
    go
    gcc
    gnumake
    lua
    elixir
    nodejs
    tree-sitter
    luaPackages.luarocks
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
    jetbrains.pycharm-professional
    docker-compose
    postgresql_13

    # Lsps
    lua-language-server
    # nodePackages.vscode-langservers-extracted
    # nodePackages.dockerfile-language-server-nodejs
    # nodePackages.typescript-language-server
    # nodePackages.yaml-language-server
    # nodePackages.pyright
    # nodePackages.volar
    texlab
    # sqls
    # efm-langserver
    # rnix-lsp
    # rust-analyzer
    # elixir-ls

    # Code formatters
    stylua
    yamlfmt
    black
    nodePackages.sql-formatter
    stylish-haskell
    alejandra
    texlive.combined.scheme-full

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
