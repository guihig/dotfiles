{self, ...}: {
  flake.modules.homeManager.ferreira-packages = {
    config,
    pkgs,
    ...
  }: {
    home.packages = with pkgs; [
      # General
      google-chrome
      chromium
      spotify
      unstable.discord
      zathura
      libreoffice
      gimp
      audacity
      obs-studio
      appimage-run

      # Audio
      pavucontrol
      alsa-utils
      vlc
      noisetorch

      # Codecs
      ffmpeg

      # Code packages
      unstable.claude-code
      cargo
      rustc
      go
      gcc
      gnumake
      lua
      luajitPackages.luarocks
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
      localtunnel

      # Code formatters
      stylua
      yamlfmt
      black
      sql-formatter
      prettier
      stylish-haskell
      alejandra
      texlive.combined.scheme-full

      # Editors
      vim
      nano
      vscode
      jetbrains.pycharm

      # Network/Vpns
      openfortivpn
      networkmanagerapplet

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
      hyfetch
      cmatrix
      libnotify
      seahorse
      gparted
      ksnip
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
  };

  flake.modules.nixos.ferreira-packages = {pkgs, ...}: {
    home-manager.sharedModules = [
      self.modules.homeManager.ferreira-packages
    ];
  };
}
