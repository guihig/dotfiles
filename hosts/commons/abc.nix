{pkgs, ...}: {
  # Time Config
  time = {
    timeZone = "America/Sao_Paulo";
    hardwareClockInLocalTime = true;
  };

  # I18n Config
  i18n = {
    defaultLocale = "en_US.UTF-8";
    extraLocaleSettings = {
      LC_ADDRESS = "pt_BR.UTF-8";
      LC_IDENTIFICATION = "pt_BR.UTF-8";
      LC_MEASUREMENT = "pt_BR.UTF-8";
      LC_MONETARY = "pt_BR.UTF-8";
      LC_NAME = "pt_BR.UTF-8";
      LC_NUMERIC = "pt_BR.UTF-8";
      LC_PAPER = "pt_BR.UTF-8";
      LC_TELEPHONE = "pt_BR.UTF-8";
      LC_TIME = "pt_BR.UTF-8";
    };
  };

  networking = {
    networkmanager.enable = true;
    firewall = {
      enable = false;
      allowedTCPPorts = [3000];
    };
  };

  # Env config
  environment = {
    variables = {
      SDL_VIDEO_MINIMIZE_ON_FOCUS_LOSS = 0;
      SHELL = "fish";
    };
    systemPackages = with pkgs; [
      firefox
      git
      wget
      curl
      vim
      neovim
      home-manager
      polkit_gnome
      gnome-keyring
      file-roller
      kitty
      lxqt.lxqt-policykit
      clamav
      chkrootkit
      SDL2
    ];
    etc."ppp/options".text = "ipcp-accept-remote";
  };

  # Extra
  virtualisation.docker.enable = true;

  # Create fonts dir
  fonts.fontDir.enable = true;

  # XDG Portal
  xdg.portal = {
    enable = true;
    extraPortals = [pkgs.xdg-desktop-portal-gtk];
    config.common.default = "gtk";
  };
}
